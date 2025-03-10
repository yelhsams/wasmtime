use crate::preview2::filesystem::FileInputStream;
use crate::preview2::poll::Subscribe;
use anyhow::Result;
use bytes::Bytes;

/// Host trait for implementing the `wasi:io/streams.input-stream` resource: A
/// bytestream which can be read from.
#[async_trait::async_trait]
pub trait HostInputStream: Subscribe {
    /// Reads up to `size` bytes, returning a buffer holding these bytes on
    /// success.
    ///
    /// This function does not block the current thread and is the equivalent of
    /// a non-blocking read. On success all bytes read are returned through
    /// `Bytes`, which is no larger than the `size` provided. If the returned
    /// list of `Bytes` is empty then no data is ready to be read at this time.
    ///
    /// # Errors
    ///
    /// The [`StreamError`] return value communicates when this stream is
    /// closed, when a read fails, or when a trap should be generated.
    fn read(&mut self, size: usize) -> Result<Bytes, StreamError>;

    /// Same as the `read` method except that bytes are skipped.
    ///
    /// Note that this method is non-blocking like `read` and returns the same
    /// errors.
    fn skip(&mut self, nelem: usize) -> Result<usize, StreamError> {
        let bs = self.read(nelem)?;
        Ok(bs.len())
    }
}

#[derive(Debug)]
pub enum StreamError {
    Closed,
    LastOperationFailed(anyhow::Error),
    Trap(anyhow::Error),
}
impl std::fmt::Display for StreamError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StreamError::Closed => write!(f, "closed"),
            StreamError::LastOperationFailed(e) => write!(f, "last operation failed: {e}"),
            StreamError::Trap(e) => write!(f, "trap: {e}"),
        }
    }
}
impl std::error::Error for StreamError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            StreamError::Closed => None,
            StreamError::LastOperationFailed(e) | StreamError::Trap(e) => e.source(),
        }
    }
}

/// Host trait for implementing the `wasi:io/streams.output-stream` resource:
/// A bytestream which can be written to.
#[async_trait::async_trait]
pub trait HostOutputStream: Subscribe {
    /// Write bytes after obtaining a permit to write those bytes
    ///
    /// Prior to calling [`write`](Self::write) the caller must call
    /// [`check_write`](Self::check_write), which resolves to a non-zero permit
    ///
    /// This method must never block.  The [`check_write`](Self::check_write)
    /// permit indicates the maximum amount of bytes that are permitted to be
    /// written in a single [`write`](Self::write) following the
    /// [`check_write`](Self::check_write) resolution.
    ///
    /// # Errors
    ///
    /// Returns a [`StreamError`] if:
    /// - stream is closed
    /// - prior operation ([`write`](Self::write) or [`flush`](Self::flush)) failed
    /// - caller performed an illegal operation (e.g. wrote more bytes than were permitted)
    fn write(&mut self, bytes: Bytes) -> Result<(), StreamError>;

    /// Trigger a flush of any bytes buffered in this stream implementation.
    ///
    /// This method may be called at any time and must never block.
    ///
    /// After this method is called, [`check_write`](Self::check_write) must
    /// pend until flush is complete.
    ///
    /// When [`check_write`](Self::check_write) becomes ready after a flush,
    /// that guarantees that all prior writes have been flushed from the
    /// implementation successfully, or that any error associated with those
    /// writes is reported in the return value of [`flush`](Self::flush) or
    /// [`check_write`](Self::check_write)
    ///
    /// # Errors
    ///
    /// Returns a [`StreamError`] if:
    /// - stream is closed
    /// - prior operation ([`write`](Self::write) or [`flush`](Self::flush)) failed
    /// - caller performed an illegal operation (e.g. wrote more bytes than were permitted)
    fn flush(&mut self) -> Result<(), StreamError>;

    /// Returns the number of bytes that are ready to be written to this stream.
    ///
    /// Zero bytes indicates that this stream is not currently ready for writing
    /// and `ready()` must be awaited first.
    ///
    /// Note that this method does not block.
    ///
    /// # Errors
    ///
    /// Returns an [`StreamError`] if:
    /// - stream is closed
    /// - prior operation ([`write`](Self::write) or [`flush`](Self::flush)) failed
    fn check_write(&mut self) -> Result<usize, StreamError>;

    /// Repeatedly write a byte to a stream.
    /// Important: this write must be non-blocking!
    /// Returning an Err which downcasts to a [`StreamError`] will be
    /// reported to Wasm as the empty error result. Otherwise, errors will trap.
    fn write_zeroes(&mut self, nelem: usize) -> Result<(), StreamError> {
        // TODO: We could optimize this to not allocate one big zeroed buffer, and instead write
        // repeatedly from a 'static buffer of zeros.
        let bs = Bytes::from_iter(core::iter::repeat(0 as u8).take(nelem));
        self.write(bs)?;
        Ok(())
    }

    /// Simultaneously waits for this stream to be writable and then returns how
    /// much may be written or the last error that happened.
    async fn write_ready(&mut self) -> Result<usize, StreamError> {
        self.ready().await;
        self.check_write()
    }
}

#[async_trait::async_trait]
impl Subscribe for Box<dyn HostOutputStream> {
    async fn ready(&mut self) {
        (**self).ready().await
    }
}

pub enum InputStream {
    Host(Box<dyn HostInputStream>),
    File(FileInputStream),
}

#[async_trait::async_trait]
impl Subscribe for InputStream {
    async fn ready(&mut self) {
        match self {
            InputStream::Host(stream) => stream.ready().await,
            // Files are always ready
            InputStream::File(_) => {}
        }
    }
}

pub type OutputStream = Box<dyn HostOutputStream>;
