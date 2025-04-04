use clap::Parser;
use dbus::blocking::Connection;
use eyre::Context;

mod focus;
pub(crate) mod focus_ext;
mod windows;
pub(crate) mod windows_ext;

#[derive(Parser)]
struct Args {
    title: String,

    #[clap(short, long)]
    launch: Option<String>,
}

fn main() -> eyre::Result<()> {
    let _ = color_eyre::install();

    let args = Args::parse();

    let conn = Connection::new_session().wrap_err("could not get D-Bus session")?;
    let open_windows = windows::list_windows(&conn).wrap_err("listing windows")?;

    for window in &open_windows {
        if !window
            .class
            .as_ref()
            .map(|cls| cls.to_lowercase().contains(&args.title))
            .unwrap_or_default()
            && !window
                .title
                .as_ref()
                .map(|cls| cls.to_lowercase().contains(&args.title))
                .unwrap_or_default()
        {
            continue;
        }

        // the command is in the title but the application is a terminal, which may print the
        // command in the title
        if args.title.to_lowercase().contains(&args.title)
            && window
                .class
                .as_ref()
                .map(|cls| cls.to_lowercase() == "alacritty")
                .unwrap_or_default()
            && args.title != "alacritty"
        {
            continue;
        }

        // found match to focus
        if window.focus {
            eprintln!("window {} already focused", args.title);
            return Ok(());
        }

        focus::focus_window(&conn, window).wrap_err("focusing window")?;
        return Ok(());
    }

    Err(eyre::eyre!("could not find running command {}", args.title))
}
