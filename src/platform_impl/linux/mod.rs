#![cfg(free_unix)]

#[cfg(all(not(x11_platform), not(wayland_platform)))]
compile_error!("Please select a feature to build for unix: `x11`, `wayland`");

use std::env;
use std::num::{NonZeroU16, NonZeroU32};
use std::os::unix::io::{AsFd, AsRawFd, BorrowedFd, RawFd};
use std::time::Duration;

use smol_str::SmolStr;

pub(crate) use self::common::xkb::{physicalkey_to_scancode, scancode_to_physicalkey};
use crate::application::ApplicationHandler;
pub(crate) use crate::cursor::OnlyCursorImageSource as PlatformCustomCursorSource;
use crate::dpi::{PhysicalPosition, PhysicalSize};
use crate::error::{EventLoopError, NotSupportedError};
use crate::event_loop::ActiveEventLoop;
pub(crate) use crate::icon::RgbaIcon as PlatformIcon;
use crate::keyboard::Key;
use crate::platform::pump_events::PumpStatus;
use crate::window::ActivationToken;

pub(crate) mod common;
#[cfg(wayland_platform)]
pub(crate) mod wayland;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Backend {
    #[cfg(wayland_platform)]
    Wayland,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct PlatformSpecificEventLoopAttributes {
    pub(crate) forced_backend: Option<Backend>,
    pub(crate) any_thread: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ApplicationName {
    pub general: String,
    pub instance: String,
}

impl ApplicationName {
    pub fn new(general: String, instance: String) -> Self {
        Self { general, instance }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlatformSpecificWindowAttributes {
    pub name: Option<ApplicationName>,
    pub activation_token: Option<ActivationToken>,
}

#[cfg_attr(not(x11_platform), allow(clippy::derivable_impls))]
impl Default for PlatformSpecificWindowAttributes {
    fn default() -> Self {
        Self { name: None, activation_token: None }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum MonitorHandle {
    #[cfg(wayland_platform)]
    Wayland(wayland::MonitorHandle),
}

/// `x11_or_wayland!(match expr; Enum(foo) => foo.something())`
/// expands to the equivalent of
/// ```ignore
/// match self {
///    Enum::X(foo) => foo.something(),
///    Enum::Wayland(foo) => foo.something(),
/// }
/// ```
/// The result can be converted to another enum by adding `; as AnotherEnum`
macro_rules! x11_or_wayland {
    (match $what:expr; $enum:ident ( $($c1:tt)* ) => $x:expr; as $enum2:ident ) => {
        match $what {
            #[cfg(wayland_platform)]
            $enum::Wayland($($c1)*) => $enum2::Wayland($x),
        }
    };
    (match $what:expr; $enum:ident ( $($c1:tt)* ) => $x:expr) => {
        match $what {
            #[cfg(wayland_platform)]
            $enum::Wayland($($c1)*) => $x,
        }
    };
}

impl MonitorHandle {
    #[inline]
    pub fn name(&self) -> Option<String> {
        x11_or_wayland!(match self; MonitorHandle(m) => m.name())
    }

    #[inline]
    pub fn native_identifier(&self) -> u32 {
        x11_or_wayland!(match self; MonitorHandle(m) => m.native_identifier())
    }

    #[inline]
    pub fn position(&self) -> Option<PhysicalPosition<i32>> {
        x11_or_wayland!(match self; MonitorHandle(m) => m.position())
    }

    #[inline]
    pub fn scale_factor(&self) -> f64 {
        x11_or_wayland!(match self; MonitorHandle(m) => m.scale_factor() as _)
    }

    #[inline]
    pub fn current_video_mode(&self) -> Option<VideoModeHandle> {
        x11_or_wayland!(match self; MonitorHandle(m) => m.current_video_mode())
    }

    #[inline]
    pub fn video_modes(&self) -> Box<dyn Iterator<Item = VideoModeHandle>> {
        x11_or_wayland!(match self; MonitorHandle(m) => Box::new(m.video_modes()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VideoModeHandle {
    #[cfg(wayland_platform)]
    Wayland(wayland::VideoModeHandle),
}

impl VideoModeHandle {
    #[inline]
    pub fn size(&self) -> PhysicalSize<u32> {
        x11_or_wayland!(match self; VideoModeHandle(m) => m.size())
    }

    #[inline]
    pub fn bit_depth(&self) -> Option<NonZeroU16> {
        x11_or_wayland!(match self; VideoModeHandle(m) => m.bit_depth())
    }

    #[inline]
    pub fn refresh_rate_millihertz(&self) -> Option<NonZeroU32> {
        x11_or_wayland!(match self; VideoModeHandle(m) => m.refresh_rate_millihertz())
    }

    #[inline]
    pub fn monitor(&self) -> MonitorHandle {
        x11_or_wayland!(match self; VideoModeHandle(m) => m.monitor(); as MonitorHandle)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct KeyEventExtra {
    pub text_with_all_modifiers: Option<SmolStr>,
    pub key_without_modifiers: Key,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum PlatformCustomCursor {
    #[cfg(wayland_platform)]
    Wayland(wayland::CustomCursor),
}

pub enum EventLoop {
    #[cfg(wayland_platform)]
    Wayland(Box<wayland::EventLoop>),
}

impl EventLoop {
    pub(crate) fn new(
        attributes: &PlatformSpecificEventLoopAttributes,
    ) -> Result<Self, EventLoopError> {
        if !attributes.any_thread && !is_main_thread() {
            panic!(
                "Initializing the event loop outside of the main thread is a significant \
                 cross-platform compatibility hazard. If you absolutely need to create an \
                 EventLoop on a different thread, you can use the \
                 `EventLoopBuilderExtX11::with_any_thread` or \
                 `EventLoopBuilderExtWayland::with_any_thread` functions."
            );
        }

        // NOTE: Wayland first because of X11 could be present under Wayland as well. Empty
        // variables are also treated as not set.
        let backend = match (
            attributes.forced_backend,
            env::var("WAYLAND_DISPLAY")
                .ok()
                .filter(|var| !var.is_empty())
                .or_else(|| env::var("WAYLAND_SOCKET").ok())
                .filter(|var| !var.is_empty())
                .is_some(),
            env::var("DISPLAY").map(|var| !var.is_empty()).unwrap_or(false),
        ) {
            // User is forcing a backend.
            (Some(backend), ..) => backend,
            // Wayland is present.
            #[cfg(wayland_platform)]
            (None, true, _) => Backend::Wayland,
            // No backend is present.
            (_, wayland_display, x11_display) => {
                let msg = if wayland_display && !cfg!(wayland_platform) {
                    "DISPLAY is not set; note: enable the `winit/wayland` feature to support \
                     Wayland"
                } else if x11_display && !cfg!(x11_platform) {
                    "neither WAYLAND_DISPLAY nor WAYLAND_SOCKET is set; note: enable the \
                     `winit/x11` feature to support X11"
                } else {
                    "neither WAYLAND_DISPLAY nor WAYLAND_SOCKET nor DISPLAY is set."
                };
                return Err(NotSupportedError::new(msg).into());
            },
        };

        // Create the display based on the backend.
        match backend {
            #[cfg(wayland_platform)]
            Backend::Wayland => EventLoop::new_wayland_any_thread().map_err(Into::into),
        }
    }

    #[cfg(wayland_platform)]
    fn new_wayland_any_thread() -> Result<EventLoop, EventLoopError> {
        wayland::EventLoop::new().map(|evlp| EventLoop::Wayland(Box::new(evlp)))
    }

    #[inline]
    pub fn is_wayland(&self) -> bool {
        match *self {
            #[cfg(wayland_platform)]
            EventLoop::Wayland(_) => true,
        }
    }

    pub fn run_with_gtk<A: ApplicationHandler>(self, app: A) -> Result<(), EventLoopError> {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.run_with_gtk(app))
    }

    pub fn run_app<A: ApplicationHandler>(self, app: A) -> Result<(), EventLoopError> {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.run_app(app))
    }

    pub fn run_app_on_demand<A: ApplicationHandler>(
        &mut self,
        app: A,
    ) -> Result<(), EventLoopError> {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.run_app_on_demand(app))
    }

    pub fn pump_app_events<A: ApplicationHandler>(
        &mut self,
        timeout: Option<Duration>,
        app: A,
    ) -> PumpStatus {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.pump_app_events(timeout, app))
    }

    pub fn window_target(&self) -> &dyn ActiveEventLoop {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.window_target())
    }
}

impl AsFd for EventLoop {
    fn as_fd(&self) -> BorrowedFd<'_> {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.as_fd())
    }
}

impl AsRawFd for EventLoop {
    fn as_raw_fd(&self) -> RawFd {
        x11_or_wayland!(match self; EventLoop(evlp) => evlp.as_raw_fd())
    }
}

/// Returns the minimum `Option<Duration>`, taking into account that `None`
/// equates to an infinite timeout, not a zero timeout (so can't just use
/// `Option::min`)
fn min_timeout(a: Option<Duration>, b: Option<Duration>) -> Option<Duration> {
    a.map_or(b, |a_timeout| b.map_or(Some(a_timeout), |b_timeout| Some(a_timeout.min(b_timeout))))
}

#[cfg(target_os = "linux")]
fn is_main_thread() -> bool {
    rustix::thread::gettid() == rustix::process::getpid()
}

#[cfg(any(target_os = "dragonfly", target_os = "freebsd", target_os = "openbsd"))]
fn is_main_thread() -> bool {
    use libc::pthread_main_np;

    unsafe { pthread_main_np() == 1 }
}

#[cfg(target_os = "netbsd")]
fn is_main_thread() -> bool {
    std::thread::current().name() == Some("main")
}
