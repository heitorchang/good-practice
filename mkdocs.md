# MkDocs

MkDocs collects Markdown files and generates a static site. The site is configured in a YAML file.

Blocks in YAML are indented with 4 spaces, not tabs. 

## Multi-level menu items

Folders in the navigation are simple labels; a link to a relevant page must be included as a subitem

The root directory for the Markdown documentation files is `docs/`

```
site_name: The History of Transportation

nav:
    - Home: 'index.md'
    - Vehicles:
        - Trucks: 'vehicles/trucks/index.md'
        - Cars:
            - Overview: 'vehicles/cars/index.md'
            - Economy: 'vehicles/cars/economy/index.md'
            - 'High-powered Sports Cars':
                - Red: 'vehicles/cars/sports/red/index.md'

```

## Search

By default, search strings must be at least 3 characters long. To change this setting, add the following to `mkdocs.yml`

```
plugins:
    - search
        min_search_length: 2
```

## Building

Run `mkdocs build` then serve the `site/` directory.

For offline use, add `use_directory_urls: false` to `mkdocs.yml` because offline navigation will not automatically load `index.html`

## Themes

`pip install mkdocs-bootswatch` to get several new themes.

[See previews here](https://mkdocs.github.io/mkdocs-bootswatch/)

Add to `mkdocs.yml`

```
theme: spacelab
```
