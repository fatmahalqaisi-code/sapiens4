require_relative 'support/desktop'
require_relative 'support/proxy'

RSpec.describe 'Workspace polling visibility', :desktop do
  it 'stops state polling while minimized and resumes without duplicate polling loops' do
    skip 'macOS window minimization contract' unless RUBY_PLATFORM.include?('darwin')
    @host = desktop_host
    requests = Queue.new
    proxy = HostProxy.new(host) do |request, response|
      requests << Process.clock_gettime(Process::CLOCK_MONOTONIC) if request.path.end_with?('/api/state')
      response
    end
    start_desktop(proxy.url)
    desktop.see('Message ChiefSapi')
    desktop.fill('Message ChiefSapi', 'Keep this unsent draft')
    Contract.wait { requests.size >= 2 }
    minimize = desktop.tree.find { |node| node['subrole'] == 'AXMinimizeButton' }
    raise 'Fixture window has no minimize control' unless minimize
    desktop.click_node(minimize)
    # Allow the already in-flight refresh and visibility event to settle.
    sleep 2
    stopped = requests.size
    sleep 3
    expect(requests.size).to eq(stopped)
    # Native fixture menu action restores the window; no JS/visibility injection.
    desktop.control('activate')
    desktop.key('command+1')
    Contract.wait { requests.size > stopped }
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep this unsent draft')
    count = requests.size
    sleep 3
    expect(requests.size - count).to be_between(1, 5)
  ensure
    # Stop the UI before removing the proxy it is polling.
    @desktop&.close
    @desktop = nil
    proxy&.close
  end
end
