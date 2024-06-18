use kernel::thread::Thread;
use kernel::time::{Duration, Instant};

pub(crate) async fn wait_timeout(deadline: Instant) {
    while Instant::now() <= deadline {
        Thread::current().sleep(Duration::from_millis(10));
        embassy_futures::yield_now().await;
    }
}