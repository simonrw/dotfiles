use std::time::Duration;

use crate::windows_ext::OrgGnomeShellExtensionsWindowsExt;
use dbus::blocking::Connection;
use eyre::Context;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Window {
    pub class: Option<String>,
    pub title: Option<String>,
    #[serde(rename = "pid")]
    _pid: u64,
    #[serde(rename = "id")]
    _id: u64,
    #[serde(rename = "maximized")]
    _maximized: u64,
    pub focus: bool,
}

pub fn list_windows(conn: &Connection) -> eyre::Result<Vec<Window>> {
    let p = conn.with_proxy(
        "org.gnome.Shell",
        "/org/gnome/Shell/Extensions/WindowsExt",
        Duration::from_secs(5),
    );
    let res = p.list().wrap_err("listing windows")?;
    dbg!(&res);
    let windows = serde_json::from_str(&res).wrap_err("deserializing JSON message")?;
    Ok(windows)
}
