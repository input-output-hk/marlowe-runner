module Component.Footer where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM
import Web.DOM.MutationRecord (target)

footer :: JSX
footer =
  DOM.footer
    { className: "footer mt-auto py-3 bg-light shadow-top"
    , children:
        [ DOM.div
            { className: "container"
            , children:
                [ DOM.span
                    { className: "text-muted d-flex justify-content-around"
                    , children:
                        [ DOM.a
                            { className: "btn btn-link text-decoration-underline-hover text-decoration-none font-weight-bold"
                            , href: "/about"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-info-circle-fill" }
                                , DOM.text "  About"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-decoration-underline-hover text-decoration-none font-weight-bold"
                            , href: "http://marlowescan.com"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-globe2" }
                                , DOM.text "  Marlowe Explorer"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-decoration-underline-hover text-decoration-none font-weight-bold"
                            , href: "https://docs.marlowe.iohk.io"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-book" }
                                , DOM.text "  Marlowe Documentation"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-decoration-underline-hover text-decoration-none font-weight-bold"
                            , href: "https://github.com/input-output-hk/marlowe-cardano"
                            , target: "_blank"
                            , children:
                                [ DOM.i { className: "h5 bi-github" }
                                , DOM.text "  Marlowe Cardano Github"
                                ]
                            }
                        , DOM.a
                            { className: "btn btn-link text-decoration-underline-hover text-decoration-none font-weight-bold"
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
