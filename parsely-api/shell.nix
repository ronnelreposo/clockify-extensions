let
  pkgs = import <nixpkgs> { config = { allowUnfree = true; }; };
in
  pkgs.mkShell {

    # Define packages
    buildInputs = [
      pkgs.which
      pkgs.stack
      pkgs.vscode
    ];
    
    # Shell environments
    env = {
      API_PORT = "8080";
      USER_ID = "TODO. DEFINE WORKSPACE USER ID";
      WORKSPACE_ID = "TODO. DEFINE WORKSPACE ID";
      # *** Erase this info, do not commit.
      PERSONALWORK_CLOCKIFY_API_KEY = "TODO. DEFINE API KEY"; 
    };

    # Export to use the variable on shell child processes.
    # You can also execute shell on shell-nix entry such as creating directories.
    shellHook = ''
      echo "Hello! Your nix environment is working."
      export API_PORT
      export USER_ID
      export WORKSPACE_ID
      export CLOCKIFY_API_KEY
    '';
  }
