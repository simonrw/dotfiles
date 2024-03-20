{
  pkgs,
  isDarwin,
  ...
}: {
  programs.firefox = {
    enable = true;
    package =
      if isDarwin
      then null
      else pkgs.firefox;
    policies = {
      DisablePocket = true;
      DisableTelemetry = true;
    };
    profiles = {
      simon = {
        name = "simon";
        extensions = with pkgs.nur.repos.rycee.firefox-addons;
          [
            enhancer-for-youtube
            facebook-container
            omnivore
            pkgs.nur.repos.rycee.firefox-addons."1password-x-password-manager"
            react-devtools
            ublock-origin
            vimium
            istilldontcareaboutcookies
            clearurls
            refined-github
          ]
          ++ (with pkgs.nur.repos.meain.firefox-addons; [
            containerise
          ]);
        extraConfig = builtins.readFile ./firefox-preferences.js;
        isDefault = true;
        settings = {
          "browser.search.region" = "GB";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
        };
      };
    };
  };
}
