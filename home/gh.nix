{pkgs, ...}: {
  programs.gh = {
    enable = true;
    settings = {
      version = "1";
      editor = "code -w";
      git_protocol = "ssh";
      prompt = "enabled";
      pager = "delta";
      aliases = {
        inbox = "api notifications --template '{{range .}}{{tablerow .subject.title .subject.url}}{{end}}'";
      };
    };
    extensions = [
      pkgs.gh-dash
      pkgs.gh-repo-url
      pkgs.gh-rebase-pr
      pkgs.gh-markdown-preview
    ];
  };

  programs.gh-dash = {
    enable = true;
    settings = {
      prSections = [
        {
          title = "LS: My Pull Requests";
          filters = "is:pr is:open author:@me org:localstack";
          layout.author.hidden = true;
        }
        {
          title = "LS: Review requested";
          filters = "is:pr is:open review-requested:@me org:localstack";
        }
        {
          title = "LS: Involved";
          filters = "is:pr is:open involves:@me -author:@me org:localstack";
        }
      ];
      issuesSections = [
        {
          title = "LS: Involved";
          filters = "is:issue is:open involves:@me org:localstack";
        }
      ];
      repoPaths = {
        "localstack/*" = "~/work/localstack/*";
      };
      pager.diff = "delta";
      keybindings = {
        prs = [
          # disable marking as ready
          {
            key = "w";
            command = ''
              gh pr view -w {{.PrNumber}} -R {{.RepoName}}
            '';
          }
        ];
      };
    };
  };
}
