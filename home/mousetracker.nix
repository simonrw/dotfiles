{ system, ... }:
let
  device = {
    x86_64-linux = "/dev/input/by-id/usb-Logitech_G203_LIGHTSYNC_Gaming_Mouse_205935534B58-event-mouse";
  }.${system};
in
{
  mousetracker = {
    inherit device;
    enable = true;
  };
}
