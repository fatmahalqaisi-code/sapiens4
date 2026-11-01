require 'time'

RSpec.describe 'Idle process profiling', :performance do
  before do
    skip 'Set SAPIENS_PROFILE=1 for opt-in process profiling' unless ENV['SAPIENS_PROFILE'] == '1'
    skip 'CPU probe currently uses macOS/Linux ps' if Gem.win_platform?
  end

  def cpu_seconds(pid)
    raw, error, status = Open3.capture3('ps', '-p', pid.to_s, '-o', 'time=')
    raise "Cannot measure CPU for #{pid}: #{error}" unless status.success? && !raw.strip.empty?
    days, clock = raw.strip.include?('-') ? raw.strip.split('-', 2) : ['0', raw.strip]
    days.to_i * 86_400 + clock.split(':').reverse.each_with_index.sum { |part, index| part.to_f * 60**index }
  end

  def monotonic; Process.clock_gettime(Process::CLOCK_MONOTONIC); end

  [0, 272, 1200].each do |turn_count|
    ['paused polling', 'active polling', 'active without clients'].each do |scenario|
      it "profiles #{scenario} with 12 agents and #{turn_count} settled turns" do
        binary = File.expand_path(ENV.fetch('SAPIENS_PROFILE_BINARY', Contract::BINARY))
        fixture = Host.new(command: [binary])
        owners = [fixture.state.fetch('main_agent_id')] + 11.times.map { |i| fixture.agent("Profile#{i}") }
        identity = fixture.api('GET', '/api/corpora')
        fixture.api('PUT', '/api/corpora/lifecycle', action: 'pause', operation: 'profile-setup', revision: identity['revision'])
        owners.each_with_index do |owner, index|
          path = File.join(fixture.root, 'agentpy/agents', owner, 'state.json')
          saved = JSON.parse(File.read(path))
          turns = (index...turn_count).step(owners.length).map do |number|
            {id: "profile-#{number}", flow: 'chat', input: 'Saved request ' * 20, status: 'failed',
              error: 'Settled fixture', created: Time.at(1_700_000_000 + number).utc.iso8601}
          end
          Contract.atomic_json(path, saved.merge('turns' => turns))
        end
        3.times { fixture.state }
        unless scenario.start_with?('paused')
          identity = fixture.api('GET', '/api/corpora')
          fixture.api('PUT', '/api/corpora/lifecycle', action: 'activate', operation: 'profile-start', revision: identity['revision'])
        end
        sleep 2
        duration = Float(ENV.fetch('SAPIENS_PROFILE_SECONDS', '30'))
        raise 'Profile duration must be at least 5 seconds' if duration < 5
        latencies, bytes = [], 0
        cpu_start = cpu_seconds(fixture.process.pid)
        started = monotonic
        deadline = started + duration
        # No host.idle, observer DB connection, or readiness polling in this window.
        while monotonic < deadline
          if scenario.end_with?('without clients')
            sleep [deadline - monotonic, 0].max
          else
            request_started = monotonic
            state = fixture.state
            latencies << monotonic - request_started
            bytes += JSON.generate(state).bytesize
            sleep [[deadline - monotonic, 0].max, 0.75].min
          end
        end
        cpu = cpu_seconds(fixture.process.pid) - cpu_start
        elapsed = monotonic - started
        ordered = latencies.sort
        report = {binary: binary, scenario: scenario, agents: owners.length, turns: turn_count,
          elapsed_seconds: elapsed.round(3), cpu_seconds: cpu.round(3), cpu_percent: (100 * cpu / elapsed).round(2),
          requests: latencies.length, response_bytes: bytes,
          p50_ms: ordered.empty? ? nil : (ordered[(ordered.length - 1) / 2] * 1000).round(2),
          p95_ms: ordered.empty? ? nil : (ordered[(ordered.length * 0.95).ceil - 1] * 1000).round(2)}
        RSpec.configuration.reporter.message("PROFILE #{JSON.generate(report)}")
        if ENV['SAPIENS_PROFILE_OUTPUT']
          File.open(ENV['SAPIENS_PROFILE_OUTPUT'], 'a') { |file| file.puts(JSON.generate(report)) }
        end
        expect(fixture.calls).to be_empty
        expect(fixture.state['turns'].length).to eq(turn_count)
      ensure
        fixture&.close
      end
    end
  end
end
