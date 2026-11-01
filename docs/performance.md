# Idle performance

This modification reduces idle database churn by reusing a serialized connection,
transacting each agent projection, and avoiding updates when projected values
have not changed. Saved JSON remains authoritative and is validated on refresh;
external edits do not need to increment a revision. Pulse history is limited in
SQL and turn metadata is matched by key. The UI stops scheduling state polls
while hidden and rejects stale polling timers after an explicit refresh.

The scheduler's pulse persistence and the full state response remain unchanged.
This is not a guarantee of a specific battery-life improvement.

## Behavioral regression checks

```sh
ruby submodules/libs4/ruby/bundle.rb exec rspec \
  spec/idle_storage_spec.rb spec/domain_spec.rb spec/corpora_spec.rb \
  spec/service_spec.rb spec/scheduling_spec.rb --profile 20
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/idle_browser_spec.rb
```

Storage tests use a paused, synthetic installation and an observer connection
to check whether repeated state reads commit changes. This observer is only for
the commit contract: it must not be present in a filesystem-churn benchmark,
because it changes SQLite's last-connection-close behavior. A storage failure
fixture verifies rollback of turn details and status events together.

The browser test minimizes the actual macOS fixture window, observes requests
at an HTTP proxy, and verifies polling and the unsent draft after restoration.
The fixture exposes the same minimize control as the production window.
Windows lifecycle behavior still needs native validation on Windows.

## Opt-in process profiling

Build both revisions first. Keep each executable in its own compatible build
layout, including domain binaries, UI assets and runtime libraries. Then run
the same spec with an absolute executable path:

```sh
SAPIENS_PROFILE=1 SAPIENS_PROFILE_SECONDS=30 \
SAPIENS_PROFILE_BINARY=/absolute/path/to/.build/bin/sapiens4 \
SAPIENS_PROFILE_OUTPUT=/absolute/path/to/profile.jsonl \
  ruby submodules/libs4/ruby/bundle.rb exec rspec \
  spec/idle_performance_spec.rb --order defined --profile 20
```

The examples create 12 agents with 0, 272 or 1,200 synthetic settled turns. Each
measures paused polling, active polling and active without clients. No real
conversations or paid models are used. Profiling is opt-in; regular suite runs
report these examples as pending.

Each JSONL record contains host process CPU seconds and percent of one core,
measurement duration, completed request count, JSON response bytes, and p50/p95
request latency. These are backend metrics, not desktop renderer CPU or watts.
The HTTP driver waits 750 ms after a completed response to approximate local UI
polling. Setup, initial projection and warm-up are outside the measurement window.
Use at least five repetitions, alternating revision order on the same machine;
report all results and compare medians. Run serially without simultaneous builds
or test suites. Small samples are diagnostics, not portable timing guarantees.

Do not use `host.idle`, `host.turn`, or another state-polling readiness helper
inside the timed window. Do not leave an observer SQLite connection open. CPU is
the host process's accumulated CPU-time delta divided by monotonic wall time;
RSpec's `--profile` only ranks example durations and is not a CPU profiler.

For a desktop energy investigation, separately capture the native host, backend
and its WebKit processes, request traffic and filesystem events. Keep display
brightness, power mode and thermal conditions comparable. Stack sampling and
verbose filesystem tracing belong in separate diagnostic runs so their overhead
does not distort the benchmark. Avoid hardware-specific CPU thresholds in normal
CI; enforce deterministic behavioral contracts and review relative profiles.

Future changes should preserve crash recovery, external saved-file validation,
pause/cancel responsiveness and real-minute routine admission. Conditional/delta
snapshots, cached metadata, event-driven scheduler wakeups and less frequent
heartbeat checkpoints require their own behavior and durability tests.
