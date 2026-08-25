{
  fetchFromGitHub,
  treesheets,
  wxwidgets_3_3,
}:

let
  lobsterSrc = fetchFromGitHub {
    owner = "aardappel";
    repo = "lobster";
    rev = "v2026.6";
    hash = "sha256-EvbuvVpNlCLu+PjhHL+bP02zjz52mwIVfT600pK0ga8=";
  };
in
treesheets.overrideAttrs (oldAttrs: {
  version = "3326";

  src = fetchFromGitHub {
    owner = "aardappel";
    repo = "treesheets";
    rev = "3326";
    hash = "sha256-2l233VIrcyGCs89MeQAbvVt3OMuGp+XGhiyo2G95Q6s=";
  };

  buildInputs = [ wxwidgets_3_3 ];

  postPatch = (oldAttrs.postPatch or "") + ''
    substituteInPlace CMakeLists.txt \
      --replace-fail \
        'FetchContent_MakeAvailable(wxwidgets)' \
        $'find_package(wxWidgets 3.3.2 REQUIRED COMPONENTS aui adv core xml net)\ninclude(''${wxWidgets_USE_FILE})' \
      --replace-fail \
        'target_link_libraries(TreeSheets PRIVATE wx::aui wx::adv wx::core wx::xml wx::net)' \
        'target_link_libraries(TreeSheets PRIVATE ''${wxWidgets_LIBRARIES})'
  '';

  cmakeFlags = (oldAttrs.cmakeFlags or [ ]) ++ [
    "-DTREESHEETS_VERSION=3326"
    "-DFETCHCONTENT_SOURCE_DIR_LOBSTER=${lobsterSrc}"
  ];
})
