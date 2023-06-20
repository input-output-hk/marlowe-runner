module Component.Footer where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

newtype Fixed = Fixed Boolean

footer :: Fixed -> JSX
footer (Fixed fixed) = do
  let
    possibleFixedClass =
        if fixed then "footer " else ""
  DOM.footer
    { className: possibleFixedClass <> "mt-auto py-2 bg-light shadow-top"
    , children:
        [ DOM.div
            { className: "container"
            , children:
                [ DOM.span
                    { className: "text-muted d-flex justify-content-around"
                    , children:
                        [ DOM.a
                            { className: "btn btn-link text-muted text-primary-hover text-decoration-none"
                            , href: "/about"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-info-circle-fill" }
                                , DOM.text "  About"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-muted text-primary-hover text-decoration-none"
                            , href: "http://marlowescan.com"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-globe2" }
                                , DOM.text "  Marlowe Explorer"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-muted text-primary-hover text-decoration-none"
                            , href: "https://docs.marlowe.iohk.io"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-book" }
                                , DOM.text "  Marlowe Documentation"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-muted text-primary-hover text-decoration-none"
                            , href: "https://github.com/input-output-hk/marlowe-cardano"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-github" }
                                , DOM.text "  Marlowe Cardano Github"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-muted text-primary-hover text-decoration-none"
                            , href: "https://play.marlowe.iohk.io"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-play-circle" }
                                , DOM.text "  Marlowe Playground"
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
