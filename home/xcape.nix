{ ... }:
{
  config = {
    services.xcape = { 
      enable = true;
      mapExpression = {
        Control_L = "Escape";
      };
      timeout = 100;
    };
  };
}
