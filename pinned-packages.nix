let
  pkgs1909 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };
in
  {
    pkgs1909 = import pkgs1909 {};
  }
