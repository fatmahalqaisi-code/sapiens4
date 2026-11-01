require 'sqlite3'

RSpec.describe 'Idle storage and saved-state projection', :host do
  before do
    @owner = chief
    identity = host.api('GET', '/api/corpora')
    host.api('PUT', '/api/corpora/lifecycle', action: 'pause', operation: 'idle-storage', revision: identity['revision'])
    @file = File.join(host.root, 'agentpy/agents', @owner, 'state.json')
    @saved = JSON.parse(File.read(@file))
    @turn = {id: 'saved-call', flow: 'chat', input: 'Saved request', status: 'failed',
      error: 'Original error', created: '2026-10-27T12:00:00+00:00', output_order: 1}
    save_turn(@turn)
    host.state
  end

  def save_turn(turn)
    # Intentionally keep the same revision: external saved files are still authoritative.
    Contract.atomic_json(@file, @saved.merge('turns' => [turn]))
  end

  it 'does not commit database changes when repeatedly reading an unchanged paused workspace' do
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |observer|
      version = observer.get_first_value('PRAGMA data_version')
      5.times { expect(host.state['turns'].find { |t| t['id'] == 'saved-call' }['error']).to eq('Original error') }
      expect(observer.get_first_value('PRAGMA data_version')).to eq(version)
    end
    expect(host.calls).to be_empty
  end

  it 'projects same-revision external edits and emits each status transition only once' do
    save_turn(@turn.merge(status: 'cancelled', error: nil, output_order: 2))
    state = host.state
    row = state['turns'].find { |t| t['id'] == 'saved-call' }
    expect(row).to include('status' => 'cancelled', 'error' => nil, 'output_order' => 2)
    events = state['events'].fetch(@owner).select { |e| e['entity'] == 'saved-call' && e['event'] == 'call.cancelled' }
    expect(events.length).to eq(1)
    3.times { host.state }
    host.restart
    expect(host.state['events'].fetch(@owner).count { |e| e['entity'] == 'saved-call' && e['event'] == 'call.cancelled' }).to eq(1)
    expect(host.calls).to be_empty
  end

  it 'detects malformed saved state after a successful snapshot and recovers after repair' do
    save_turn(@turn.merge(status: 'misspelled'))
    expect(host.request('GET', '/api/state').first).to eq(500)
    expect(host.request('GET', '/api/health').first).to eq(200)
    save_turn(@turn)
    expect(host.state['turns'].find { |t| t['id'] == 'saved-call' }['status']).to eq('failed')
  end

  it 'rolls back a failed projection and can retry it without partial status events' do
    database = File.join(host.root, 'corpora.sqlite3')
    SQLite3::Database.open(database) do |db|
      db.execute("CREATE TRIGGER reject_turn BEFORE UPDATE ON turns BEGIN SELECT RAISE(ABORT, 'fixture storage failure'); END")
    end
    save_turn(@turn.merge(status: 'cancelled', output_order: 2))
    expect(host.request('GET', '/api/state').first).to eq(500)
    SQLite3::Database.open(database) do |db|
      expect(db.get_first_value("SELECT status FROM turns WHERE id='saved-call'")).to eq('failed')
      expect(JSON.parse(db.get_first_value("SELECT value FROM turn_details WHERE id='saved-call'"))['output_order']).to eq(1)
      expect(db.get_first_value("SELECT COUNT(*) FROM activity_events WHERE event='call.cancelled'")).to eq(0)
      db.execute('DROP TRIGGER reject_turn')
    end
    expect(host.state['turns'].find { |t| t['id'] == 'saved-call' }['status']).to eq('cancelled')
  end

  it 'returns only the latest 500 pulse records in descending order' do
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.transaction { 520.times { |i| db.execute('INSERT INTO pulse_calls(value) VALUES (?)', [JSON.generate(fixture: i)]) } }
    end
    expect(host.state['pulses'].map { |p| p['fixture'] }).to eq((20...520).to_a.reverse)
  end
end
