use std::io::Write;

use log;
use log::{LogRecord, LogLevelFilter, LogMetadata, SetLoggerError};

#[macro_export]
macro_rules! eprintln {
    ($($arg:tt)*) => ({
        writeln!(&mut ::std::io::stderr(), $($arg)*).unwrap()
    })
}


#[macro_export]
macro_rules! with_warn {
    ($expr: expr) => (with_warn!("{}", $expr));
    ($fmt: tt, $expr: expr) => (match $expr {
        Ok(v) => v,
        Err(e) => warn!($fmt, e),
    });
}

#[macro_export]
macro_rules! catching {
    ($expr: expr) => (catching!("{:?}", $expr));
    ($fmt: tt, $expr: expr) => (match $expr {
        Ok(v) => v,
        Err(e) => debug!($fmt, e),
    });
}

#[macro_export]
macro_rules! catching_at {
    ($log_fn: ident, $expr: expr) => ($log_fn!("{:?}", $expr));
    ($log_fn: ident, $fmt: tt, $expr: expr) => (match $expr {
        Ok(v) => v,
        Err(e) => $log_fn!($fmt, e),
    });
}


const MAX_LOG_LEVEL: LogLevelFilter = LogLevelFilter::Debug;

pub struct SimpleLogger;

impl SimpleLogger {
    pub fn init() -> Result<(), SetLoggerError> {
        log::set_logger(|max_log_level| {
            max_log_level.set(MAX_LOG_LEVEL);
            Box::new(SimpleLogger)
        })
    }
}

impl log::Log for SimpleLogger {

    fn enabled(&self, _: &LogMetadata) -> bool {
        true
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            eprintln!("[{}] [{}] {}", record.metadata().target(), record.level(), record.args());
        }
    }
}
