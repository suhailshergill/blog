{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, clientsession, conduit, containers, data-default, directory
, fast-logger, friendly-time, hamlet, hjsmin, http-conduit
, http-types, monad-control, monad-logger, mtl, old-locale
, persistent, persistent-postgresql, process, resourcet, safe
, shakespeare-css, shakespeare-js, shakespeare-text, stdenv
, template-haskell, text, time, transformers, wai, wai-extra, warp
, yaml, yesod, yesod-auth, yesod-core, yesod-default, yesod-form
, yesod-newsfeed, yesod-paginator, yesod-platform, yesod-static
, yesod-test
}:
mkDerivation {
  pname = "blog";
  version = "0.0.7";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base blaze-html blaze-markup bytestring clientsession conduit
    containers data-default directory fast-logger friendly-time hamlet
    hjsmin http-conduit http-types monad-control monad-logger mtl
    old-locale persistent persistent-postgresql process resourcet safe
    shakespeare-css shakespeare-js shakespeare-text template-haskell
    text time transformers wai wai-extra warp yaml yesod yesod-auth
    yesod-core yesod-default yesod-form yesod-newsfeed yesod-paginator
    yesod-platform yesod-static yesod-test
  ];
  testDepends = [ base yesod-test ];
  homepage = "http://blog.shergill.su/";
  description = "First attempts at a personal blog";
  license = stdenv.lib.licenses.bsd3;
}
