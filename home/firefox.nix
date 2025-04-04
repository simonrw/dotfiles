{
  pkgs,
  isDarwin,
  ...
}: {
  programs.firefox = {
    enable = false;
    package =
      if isDarwin
      then null
      else pkgs.firefox;
    policies = {
      DisablePocket = true;
      DisableTelemetry = true;
      UserMessaging = {
        ExtensionRecommendations = false;
        FeatureRecommendations = false;
        MoreFromMozilla = false;
        SkipOnboarding = true;
        WhatsNew = false;
      };
    };
    profiles = {
      simon = {
        name = "simon";
        isDefault = true;
        settings = {
          "browser.search.region" = "GB";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
        };
        extraConfig = builtins.readFile ./firefox/user.js;
      };
    };
  };
}
