{pkgs, ...}: {
  imports = [
    ./git/global.nix
  ];

  # user specific git overrides here
  programs.git = {
    userName = "Simon Walker";
    userEmail = "s.r.walker101@googlemail.com";
    includes = [
      {
        path = "~/.gitlocalconfig/local";
      }
      {
        condition = "gitdir:~/dev/";
        path = "~/dev/.gitconfig";
      }
      {
        condition = "gitdir:~/tmp/";
        path = "~/dev/.gitconfig";
      }
      {
        condition = "gitdir:/tmp/";
        path = "~/dev/.gitconfig";
      }
      {
        condition = "gitdir:~/work/localstack/";
        path = "~/work/localstack/.gitconfig";
      }
    ];
    signing = {
      gpgPath = "${pkgs.gnupg}/bin/gpg";
      key = "0x7A7C803A4612CE7C";
      signByDefault = true;
    };
  };
}
