{ config, lib, ... }:
with lib;
let
  cfg = config.me.kubeconfig;
in
{
  options.me.kubeconfig.symlink-kubeconfig-to-dev-null = mkEnableOption "symlink ~/.kube/config to /dev/null";

  config = mkIf cfg.symlink-kubeconfig-to-dev-null {
    home.file.".kube/config".source = config.lib.file.mkOutOfStoreSymlink "/dev/null";
  };
}
