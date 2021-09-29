{
  home-manager.users.alternateved.services.redshift = {
    enable = true;
    provider = "manual";
    latitude = 54.7;
    longitude = 19.4;
    temperature = {
      day = 5700;
      night = 3000;
    };
  };
}
