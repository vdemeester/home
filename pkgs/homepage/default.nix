{
  lib,
  stdenv,
  globals,
}:
let
  # Service category definitions with metadata
  serviceCategories = {
    media = {
      title = "Media Services";
      services = [
        "jellyfin"
        "jellyseerr"
        "immich"
      ];
      descriptions = {
        jellyfin = "Media server for movies, TV shows, and music";
        jellyseerr = "Request management for media";
        immich = "Photo and video backup";
      };
    };

    downloads = {
      title = "Download & Management";
      services = [
        "transmission"
        "sonarr"
        "radarr"
        "lidarr"
        "bazarr"
      ];
      descriptions = {
        transmission = "BitTorrent client";
        sonarr = "TV show management";
        radarr = "Movie management";
        lidarr = "Music management";
        bazarr = "Subtitle management";
      };
    };

    utilities = {
      title = "Utilities";
      services = [
        "kiwix"
        "n8n"
        "paperless"
        "grafana"
      ];
      descriptions = {
        kiwix = "Offline Wikipedia and content";
        n8n = "Workflow automation";
        paperless = "Document management";
        grafana = "Monitoring and metrics";
      };
    };
  };

  # Extract syncthing machines from globals
  syncthingMachines = lib.filterAttrs (
    _name: machine: machine ? syncthing && machine.syncthing ? folders
  ) globals.machines;

  # Generate syncthing service entries
  syncthingServices = [
    {
      name = "Syncthing (Overview)";
      url = "https://syncthing.sbr.pm";
      description = "File synchronization overview";
    }
  ]
  ++ (lib.mapAttrsToList (name: _machine: {
    name = "Syncthing (${name})";
    url = "https://syncthing.sbr.pm/${name}";
    description = "Syncthing on ${name}";
  }) syncthingMachines);

  # Generate service entries from service list
  mkServiceEntry =
    category: serviceName:
    let
      # Capitalize first letter
      capitalize = str: (lib.toUpper (lib.substring 0 1 str)) + (lib.substring 1 (-1) str);
      displayName = capitalize serviceName;
      description =
        category.descriptions.${serviceName} or "Service on ${globals.services.${serviceName}.host}";
    in
    {
      name = displayName;
      url = "https://${serviceName}.sbr.pm";
      inherit description;
    };

  # Generate all service entries for a category
  mkCategoryServices =
    category: map (serviceName: mkServiceEntry category serviceName) category.services;

  # Generate service list HTML
  mkServiceList =
    services:
    lib.concatMapStringsSep "\n" (service: ''
      <div class="service-card">
        <h3><a href="${service.url}">${service.name}</a></h3>
        <p>${service.description}</p>
      </div>
    '') services;

  # Generate category section HTML
  mkCategorySection =
    categoryId: category:
    let
      services = if categoryId == "sync" then syncthingServices else mkCategoryServices category;
    in
    ''
      <section class="category">
        <h2>${category.title}</h2>
        <div class="service-grid">
          ${mkServiceList services}
        </div>
      </section>
    '';

  # Generate the complete HTML page
  htmlContent = ''
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Services Dashboard</title>
      <style>
        * {
          margin: 0;
          padding: 0;
          box-sizing: border-box;
        }

        body {
          font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
          line-height: 1.6;
          color: #333;
          background: #f5f5f5;
          padding: 2rem;
        }

        .container {
          max-width: 1200px;
          margin: 0 auto;
          background: white;
          padding: 2rem;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }

        h1 {
          color: #2c3e50;
          margin-bottom: 2rem;
          padding-bottom: 1rem;
          border-bottom: 2px solid #3498db;
        }

        .category {
          margin-bottom: 3rem;
        }

        h2 {
          color: #34495e;
          margin-bottom: 1rem;
          font-size: 1.5rem;
        }

        .service-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
          gap: 1rem;
        }

        .service-card {
          background: #f8f9fa;
          padding: 1.5rem;
          border-radius: 6px;
          border-left: 4px solid #3498db;
          transition: transform 0.2s, box-shadow 0.2s;
        }

        .service-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }

        .service-card h3 {
          font-size: 1.1rem;
          margin-bottom: 0.5rem;
        }

        .service-card a {
          color: #2980b9;
          text-decoration: none;
          font-weight: 500;
        }

        .service-card a:hover {
          color: #3498db;
          text-decoration: underline;
        }

        .service-card p {
          color: #7f8c8d;
          font-size: 0.9rem;
        }

        @media (max-width: 768px) {
          body {
            padding: 1rem;
          }

          .container {
            padding: 1rem;
          }

          .service-grid {
            grid-template-columns: 1fr;
          }
        }
      </style>
    </head>
    <body>
      <div class="container">
        <h1>Services Dashboard</h1>

        ${mkCategorySection "media" serviceCategories.media}
        ${mkCategorySection "downloads" serviceCategories.downloads}
        ${mkCategorySection "sync" {
          title = "File Synchronization";
        }}
        ${mkCategorySection "utilities" serviceCategories.utilities}
      </div>
    </body>
    </html>
  '';
in
stdenv.mkDerivation {
  pname = "homepage";
  version = "1.0.0";

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cat > $out/index.html << 'EOF'
    ${htmlContent}
    EOF

    runHook postInstall
  '';

  meta = {
    description = "Simple HTML homepage listing all services from globals.nix";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
