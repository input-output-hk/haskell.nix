// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="index.html"><strong aria-hidden="true">1.</strong> Introduction</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="motivation.html"><strong aria-hidden="true">1.1.</strong> Motivation</a></li><li class="chapter-item expanded "><a href="architecture.html"><strong aria-hidden="true">1.2.</strong> Architecture</a></li></ol></li><li class="chapter-item expanded "><a href="tutorials/index.html"><strong aria-hidden="true">2.</strong> Tutorials</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="tutorials/development.html"><strong aria-hidden="true">2.1.</strong> Creating a development environment</a></li><li class="chapter-item expanded "><a href="tutorials/getting-started.html"><strong aria-hidden="true">2.2.</strong> Getting started</a></li><li class="chapter-item expanded "><a href="tutorials/clean-git.html"><strong aria-hidden="true">2.3.</strong> Sourcing files only part of git repository using cleanGit</a></li><li class="chapter-item expanded "><a href="tutorials/source-repository-hashes.html"><strong aria-hidden="true">2.4.</strong> Handling git repositories in projects</a></li><li class="chapter-item expanded "><a href="tutorials/pkg-map.html"><strong aria-hidden="true">2.5.</strong> Mapping non-Haskell dependencies to Nixpkgs</a></li><li class="chapter-item expanded "><a href="tutorials/hackage-stackage.html"><strong aria-hidden="true">2.6.</strong> Bumping Hackage and Stackage snapshots</a></li><li class="chapter-item expanded "><a href="tutorials/materialization.html"><strong aria-hidden="true">2.7.</strong> Materialization: Speeding up Nix evaluation</a></li><li class="chapter-item expanded "><a href="tutorials/cross-compilation.html"><strong aria-hidden="true">2.8.</strong> Cross-compiling your project</a></li><li class="chapter-item expanded "><a href="tutorials/coverage.html"><strong aria-hidden="true">2.9.</strong> Generating coverage information</a></li><li class="chapter-item expanded "><a href="tutorials/building-package-from-stackage-hackage.html"><strong aria-hidden="true">2.10.</strong> Build a specific package from Hackage or Stackage</a></li><li class="chapter-item expanded "><a href="tutorials/ca-derivations.html"><strong aria-hidden="true">2.11.</strong> Content addressed derivations</a></li></ol></li><li class="chapter-item expanded "><a href="reference/index.html"><strong aria-hidden="true">3.</strong> Reference</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="reference/supported-ghc-versions.html"><strong aria-hidden="true">3.1.</strong> Supported GHC versions</a></li><li class="chapter-item expanded "><a href="reference/commands.html"><strong aria-hidden="true">3.2.</strong> Command-line tools</a></li><li class="chapter-item expanded "><a href="reference/library.html"><strong aria-hidden="true">3.3.</strong> Haskell.nix Library</a></li><li class="chapter-item expanded "><a href="reference/modules.html"><strong aria-hidden="true">3.4.</strong> Module options</a></li><li class="chapter-item expanded "><a href="troubleshooting.html"><strong aria-hidden="true">3.5.</strong> Troubleshooting</a></li></ol></li><li class="chapter-item expanded "><a href="template/index.html"><strong aria-hidden="true">4.</strong> Templates / Abstraction</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="template/iohk-nix.html"><strong aria-hidden="true">4.1.</strong> IOHKs nix library</a></li></ol></li><li class="chapter-item expanded "><a href="dev/index.html"><strong aria-hidden="true">5.</strong> Dev Notes</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="dev/dev-architecture.html"><strong aria-hidden="true">5.1.</strong> Architecture</a></li><li class="chapter-item expanded "><a href="dev/installing-nix-tools.html"><strong aria-hidden="true">5.2.</strong> Installing nix-tools</a></li><li class="chapter-item expanded "><a href="dev/nix-tools-pin.html"><strong aria-hidden="true">5.3.</strong> How to update nix-tools</a></li><li class="chapter-item expanded "><a href="dev/manually-generating-nix-expressions.html"><strong aria-hidden="true">5.4.</strong> Manually generating Nix expressions</a></li><li class="chapter-item expanded "><a href="dev/maintainer-scripts.html"><strong aria-hidden="true">5.5.</strong> Maintainer Scripts</a></li><li class="chapter-item expanded "><a href="dev/nixpkgs-pin.html"><strong aria-hidden="true">5.6.</strong> Nixpkgs Pin</a></li><li class="chapter-item expanded "><a href="dev/removing-with-package-wrapper.html"><strong aria-hidden="true">5.7.</strong> Removing withPackage wrapper</a></li><li class="chapter-item expanded "><a href="dev/tests.html"><strong aria-hidden="true">5.8.</strong> Test Suite</a></li><li class="chapter-item expanded "><a href="dev/adding-new-ghc.html"><strong aria-hidden="true">5.9.</strong> Adding a new GHC version</a></li><li class="chapter-item expanded "><a href="dev/coverage.html"><strong aria-hidden="true">5.10.</strong> Coverage</a></li><li class="chapter-item expanded "><a href="dev/hix.html"><strong aria-hidden="true">5.11.</strong> Making changes to Hix</a></li><li class="chapter-item expanded "><a href="changelog.html"><strong aria-hidden="true">5.12.</strong> ChangeLog</a></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
