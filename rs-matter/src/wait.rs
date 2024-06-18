use kernel::thread::Thread;
use kernel::time::{Duration, Instant};

pub(crate) async fn wait_timeout(deadline: Instant) {
    while Instant::now() <= deadline {
        embassy_futures::yield_now().await;
        Thread::current().sleep(Duration::from_millis(10));
    }
}
