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

    p.activate_by_wm_class(&w.class)
        .wrap_err("activating window")?;

    Ok(())
}
