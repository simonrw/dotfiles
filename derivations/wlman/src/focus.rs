use std::time::Duration;

use dbus::blocking::Connection;
use eyre::Context;

use crate::windows::Window;

use crate::focus_ext::DeLucaswerkmeisterActivateWindowByTitle;

pub fn focus_window(conn: &Connection, w: &Window) -> eyre::Result<()> {
    eprintln!("focusing window {:?}", w);
    let p = conn.with_proxy(
        "org.gnome.Shell",
        "/de/lucaswerkmeister/ActivateWindowByTitle",
        Duration::from_secs(5),
    );

    if let Some(cls) = w.class.as_ref() {
        p.activate_by_wm_class(cls).wrap_err("activating window")?;
    }

    Ok(())
}
