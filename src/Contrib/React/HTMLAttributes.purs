module Contrib.React.HTMLAttributes where

import Data.Undefined.NoProblem (Opt)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- react-bootstrap components use these attribute sets
-- and they are not used by the react-basic.
-- FIXME: actually port or codegen all of them.

-- interface DOMAttributes<T> {
type DOMAttributes otherProps =
  ( children :: Array JSX
  -- , dangerouslySetInnerHTML?: {
  --       __html: string;
  --   } | undefined;

  --   // Clipboard Events
  --   onCopy?: ClipboardEventHandler<T> | undefined;
  --   onCopyCapture?: ClipboardEventHandler<T> | undefined;
  --   onCut?: ClipboardEventHandler<T> | undefined;
  --   onCutCapture?: ClipboardEventHandler<T> | undefined;
  --   onPaste?: ClipboardEventHandler<T> | undefined;
  --   onPasteCapture?: ClipboardEventHandler<T> | undefined;

  --   // Composition Events
  --   onCompositionEnd?: CompositionEventHandler<T> | undefined;
  --   onCompositionEndCapture?: CompositionEventHandler<T> | undefined;
  --   onCompositionStart?: CompositionEventHandler<T> | undefined;
  --   onCompositionStartCapture?: CompositionEventHandler<T> | undefined;
  --   onCompositionUpdate?: CompositionEventHandler<T> | undefined;
  --   onCompositionUpdateCapture?: CompositionEventHandler<T> | undefined;

  --   // Focus Events
  --   onFocus?: FocusEventHandler<T> | undefined;
  --   onFocusCapture?: FocusEventHandler<T> | undefined;
  --   onBlur?: FocusEventHandler<T> | undefined;
  --   onBlurCapture?: FocusEventHandler<T> | undefined;

  --   // Form Events
  --   onChange?: FormEventHandler<T> | undefined;
  --   onChangeCapture?: FormEventHandler<T> | undefined;
  --   onBeforeInput?: FormEventHandler<T> | undefined;
  --   onBeforeInputCapture?: FormEventHandler<T> | undefined;
  --   onInput?: FormEventHandler<T> | undefined;
  --   onInputCapture?: FormEventHandler<T> | undefined;
  --   onReset?: FormEventHandler<T> | undefined;
  --   onResetCapture?: FormEventHandler<T> | undefined;
  --   onSubmit?: FormEventHandler<T> | undefined;
  --   onSubmitCapture?: FormEventHandler<T> | undefined;
  --   onInvalid?: FormEventHandler<T> | undefined;
  --   onInvalidCapture?: FormEventHandler<T> | undefined;

  --   // Image Events
  --   onLoad?: ReactEventHandler<T> | undefined;
  --   onLoadCapture?: ReactEventHandler<T> | undefined;
  --   onError?: ReactEventHandler<T> | undefined; // also a Media Event
  --   onErrorCapture?: ReactEventHandler<T> | undefined; // also a Media Event

  --   // Keyboard Events
  --   onKeyDown?: KeyboardEventHandler<T> | undefined;
  --   onKeyDownCapture?: KeyboardEventHandler<T> | undefined;
  --   /** @deprecated */
  --   onKeyPress?: KeyboardEventHandler<T> | undefined;
  --   /** @deprecated */
  --   onKeyPressCapture?: KeyboardEventHandler<T> | undefined;
  --   onKeyUp?: KeyboardEventHandler<T> | undefined;
  --   onKeyUpCapture?: KeyboardEventHandler<T> | undefined;

  --   // Media Events
  --   onAbort?: ReactEventHandler<T> | undefined;
  --   onAbortCapture?: ReactEventHandler<T> | undefined;
  --   onCanPlay?: ReactEventHandler<T> | undefined;
  --   onCanPlayCapture?: ReactEventHandler<T> | undefined;
  --   onCanPlayThrough?: ReactEventHandler<T> | undefined;
  --   onCanPlayThroughCapture?: ReactEventHandler<T> | undefined;
  --   onDurationChange?: ReactEventHandler<T> | undefined;
  --   onDurationChangeCapture?: ReactEventHandler<T> | undefined;
  --   onEmptied?: ReactEventHandler<T> | undefined;
  --   onEmptiedCapture?: ReactEventHandler<T> | undefined;
  --   onEncrypted?: ReactEventHandler<T> | undefined;
  --   onEncryptedCapture?: ReactEventHandler<T> | undefined;
  --   onEnded?: ReactEventHandler<T> | undefined;
  --   onEndedCapture?: ReactEventHandler<T> | undefined;
  --   onLoadedData?: ReactEventHandler<T> | undefined;
  --   onLoadedDataCapture?: ReactEventHandler<T> | undefined;
  --   onLoadedMetadata?: ReactEventHandler<T> | undefined;
  --   onLoadedMetadataCapture?: ReactEventHandler<T> | undefined;
  --   onLoadStart?: ReactEventHandler<T> | undefined;
  --   onLoadStartCapture?: ReactEventHandler<T> | undefined;
  --   onPause?: ReactEventHandler<T> | undefined;
  --   onPauseCapture?: ReactEventHandler<T> | undefined;
  --   onPlay?: ReactEventHandler<T> | undefined;
  --   onPlayCapture?: ReactEventHandler<T> | undefined;
  --   onPlaying?: ReactEventHandler<T> | undefined;
  --   onPlayingCapture?: ReactEventHandler<T> | undefined;
  --   onProgress?: ReactEventHandler<T> | undefined;
  --   onProgressCapture?: ReactEventHandler<T> | undefined;
  --   onRateChange?: ReactEventHandler<T> | undefined;
  --   onRateChangeCapture?: ReactEventHandler<T> | undefined;
  --   onResize?: ReactEventHandler<T> | undefined;
  --   onResizeCapture?: ReactEventHandler<T> | undefined;
  --   onSeeked?: ReactEventHandler<T> | undefined;
  --   onSeekedCapture?: ReactEventHandler<T> | undefined;
  --   onSeeking?: ReactEventHandler<T> | undefined;
  --   onSeekingCapture?: ReactEventHandler<T> | undefined;
  --   onStalled?: ReactEventHandler<T> | undefined;
  --   onStalledCapture?: ReactEventHandler<T> | undefined;
  --   onSuspend?: ReactEventHandler<T> | undefined;
  --   onSuspendCapture?: ReactEventHandler<T> | undefined;
  --   onTimeUpdate?: ReactEventHandler<T> | undefined;
  --   onTimeUpdateCapture?: ReactEventHandler<T> | undefined;
  --   onVolumeChange?: ReactEventHandler<T> | undefined;
  --   onVolumeChangeCapture?: ReactEventHandler<T> | undefined;
  --   onWaiting?: ReactEventHandler<T> | undefined;
  --   onWaitingCapture?: ReactEventHandler<T> | undefined;

  --   // MouseEvents
  --   onAuxClick?: MouseEventHandler<T> | undefined;
  --   onAuxClickCapture?: MouseEventHandler<T> | undefined;
  --   onClick?: MouseEventHandler<T> | undefined;
  --   onClickCapture?: MouseEventHandler<T> | undefined;
  --   onContextMenu?: MouseEventHandler<T> | undefined;
  --   onContextMenuCapture?: MouseEventHandler<T> | undefined;
  --   onDoubleClick?: MouseEventHandler<T> | undefined;
  --   onDoubleClickCapture?: MouseEventHandler<T> | undefined;
  --   onDrag?: DragEventHandler<T> | undefined;
  --   onDragCapture?: DragEventHandler<T> | undefined;
  --   onDragEnd?: DragEventHandler<T> | undefined;
  --   onDragEndCapture?: DragEventHandler<T> | undefined;
  --   onDragEnter?: DragEventHandler<T> | undefined;
  --   onDragEnterCapture?: DragEventHandler<T> | undefined;
  --   onDragExit?: DragEventHandler<T> | undefined;
  --   onDragExitCapture?: DragEventHandler<T> | undefined;
  --   onDragLeave?: DragEventHandler<T> | undefined;
  --   onDragLeaveCapture?: DragEventHandler<T> | undefined;
  --   onDragOver?: DragEventHandler<T> | undefined;
  --   onDragOverCapture?: DragEventHandler<T> | undefined;
  --   onDragStart?: DragEventHandler<T> | undefined;
  --   onDragStartCapture?: DragEventHandler<T> | undefined;
  --   onDrop?: DragEventHandler<T> | undefined;
  --   onDropCapture?: DragEventHandler<T> | undefined;
  --   onMouseDown?: MouseEventHandler<T> | undefined;
  --   onMouseDownCapture?: MouseEventHandler<T> | undefined;
  --   onMouseEnter?: MouseEventHandler<T> | undefined;
  --   onMouseLeave?: MouseEventHandler<T> | undefined;
  --   onMouseMove?: MouseEventHandler<T> | undefined;
  --   onMouseMoveCapture?: MouseEventHandler<T> | undefined;
  --   onMouseOut?: MouseEventHandler<T> | undefined;
  --   onMouseOutCapture?: MouseEventHandler<T> | undefined;
  --   onMouseOver?: MouseEventHandler<T> | undefined;
  --   onMouseOverCapture?: MouseEventHandler<T> | undefined;
  --   onMouseUp?: MouseEventHandler<T> | undefined;
  --   onMouseUpCapture?: MouseEventHandler<T> | undefined;

  --   // Selection Events
  --   onSelect?: ReactEventHandler<T> | undefined;
  --   onSelectCapture?: ReactEventHandler<T> | undefined;

  --   // Touch Events
  --   onTouchCancel?: TouchEventHandler<T> | undefined;
  --   onTouchCancelCapture?: TouchEventHandler<T> | undefined;
  --   onTouchEnd?: TouchEventHandler<T> | undefined;
  --   onTouchEndCapture?: TouchEventHandler<T> | undefined;
  --   onTouchMove?: TouchEventHandler<T> | undefined;
  --   onTouchMoveCapture?: TouchEventHandler<T> | undefined;
  --   onTouchStart?: TouchEventHandler<T> | undefined;
  --   onTouchStartCapture?: TouchEventHandler<T> | undefined;

  --   // Pointer Events
  --   onPointerDown?: PointerEventHandler<T> | undefined;
  --   onPointerDownCapture?: PointerEventHandler<T> | undefined;
  --   onPointerMove?: PointerEventHandler<T> | undefined;
  --   onPointerMoveCapture?: PointerEventHandler<T> | undefined;
  --   onPointerUp?: PointerEventHandler<T> | undefined;
  --   onPointerUpCapture?: PointerEventHandler<T> | undefined;
  --   onPointerCancel?: PointerEventHandler<T> | undefined;
  --   onPointerCancelCapture?: PointerEventHandler<T> | undefined;
  --   onPointerEnter?: PointerEventHandler<T> | undefined;
  --   onPointerEnterCapture?: PointerEventHandler<T> | undefined;
  --   onPointerLeave?: PointerEventHandler<T> | undefined;
  --   onPointerLeaveCapture?: PointerEventHandler<T> | undefined;
  --   onPointerOver?: PointerEventHandler<T> | undefined;
  --   onPointerOverCapture?: PointerEventHandler<T> | undefined;
  --   onPointerOut?: PointerEventHandler<T> | undefined;
  --   onPointerOutCapture?: PointerEventHandler<T> | undefined;
  --   onGotPointerCapture?: PointerEventHandler<T> | undefined;
  --   onGotPointerCaptureCapture?: PointerEventHandler<T> | undefined;
  --   onLostPointerCapture?: PointerEventHandler<T> | undefined;
  --   onLostPointerCaptureCapture?: PointerEventHandler<T> | undefined;

  --   // UI Events
  --   onScroll?: UIEventHandler<T> | undefined;
  --   onScrollCapture?: UIEventHandler<T> | undefined;

  --   // Wheel Events
  --   onWheel?: WheelEventHandler<T> | undefined;
  --   onWheelCapture?: WheelEventHandler<T> | undefined;

  --   // Animation Events
  --   onAnimationStart?: AnimationEventHandler<T> | undefined;
  --   onAnimationStartCapture?: AnimationEventHandler<T> | undefined;
  --   onAnimationEnd?: AnimationEventHandler<T> | undefined;
  --   onAnimationEndCapture?: AnimationEventHandler<T> | undefined;
  --   onAnimationIteration?: AnimationEventHandler<T> | undefined;
  --   onAnimationIterationCapture?: AnimationEventHandler<T> | undefined;

  --   // Transition Events
  --   onTransitionEnd?: TransitionEventHandler<T> | undefined;
  --   onTransitionEndCapture?: TransitionEventHandler<T> | undefined;
  | otherProps
  )

foreign import data InputMode :: Type

-- inputMode = "none" | "text" | "tel" | "url" | "email" | "numeric" | "decimal" | "search"
inputMode
  :: { decimal :: InputMode
     , email :: InputMode
     , none :: InputMode
     , numeric :: InputMode
     , search :: InputMode
     , tel :: InputMode
     , text :: InputMode
     , url :: InputMode
     }
inputMode =
  { none: unsafeCoerce "none" :: InputMode
  , text: unsafeCoerce "text" :: InputMode
  , tel: unsafeCoerce "tel" :: InputMode
  , url: unsafeCoerce "url" :: InputMode
  , email: unsafeCoerce "email" :: InputMode
  , numeric: unsafeCoerce "numeric" :: InputMode
  , decimal: unsafeCoerce "decimal" :: InputMode
  , search: unsafeCoerce "search" :: InputMode
  }

-- interface HTMLAttributes<T> extends AriaAttributes, DOMAttributes<T> {
type HTMLAttributes otherProps =
  (
    --       about?: string | undefined;
    --       accessKey?: string | undefined;
    --       autoCapitalize?: string | undefined;
    --       autoCorrect?: string | undefined;
    --       autoSave?: string | undefined;
    className :: String
  --       color?: string | undefined;
  --       contentEditable?: Booleanish | "inherit" | undefined;
  --       contextMenu?: string | undefined;
  --       datatype?: string | undefined;
  --       defaultChecked?: boolean | undefined;
  --       defaultValue?: string | number | ReadonlyArray<string> | undefined;
  --       dir?: string | undefined;
  --       draggable?: Booleanish | undefined;
  --       hidden?: boolean | undefined;
  , id :: String
  --       inlist?: any;
  , inputMode :: InputMode
  --       is?: string | undefined;
  --       itemID?: string | undefined;
  --       itemProp?: string | undefined;
  --       itemRef?: string | undefined;
  --       itemScope?: boolean | undefined;
  --       itemType?: string | undefined;
  --       lang?: string | undefined;
  --       nonce?: string | undefined;
  , placeholder :: String
  --       prefix?: string | undefined;
  --       property?: string | undefined;
  --       radioGroup?: string | undefined; // <command>, <menuitem>
  --       resource?: string | undefined;
  --       results?: number | undefined;
  --       role?: AriaRole | undefined;
  --       security?: string | undefined;
  --       slot?: string | undefined;
  --       spellCheck?: Booleanish | undefined;
  --       style?: CSSProperties | undefined;
  --       suppressContentEditableWarning?: boolean | undefined;
  --       suppressHydrationWarning?: boolean | undefined;
  --       tabIndex?: number | undefined;
  --       title?: string | undefined;
  --       translate?: 'yes' | 'no' | undefined;
  --       typeof?: string | undefined;
  --       unselectable?: 'on' | 'off' | undefined;
  --       vocab?: string | undefined;
  --
  | DOMAttributes + otherProps
  )

--     interface InputHTMLAttributes<T> extends HTMLAttributes<T> {
--         accept?: string | undefined;
--         alt?: string | undefined;
--         autoComplete?: string | undefined;
--         autoFocus?: boolean | undefined;
--         capture?: boolean | 'user' | 'environment' | undefined; // https://www.w3.org/TR/html-media-capture/#the-capture-attribute
--         checked?: boolean | undefined;
--         crossOrigin?: string | undefined;
--         disabled?: boolean | undefined;
--         enterKeyHint?: 'enter' | 'done' | 'go' | 'next' | 'previous' | 'search' | 'send' | undefined;
--         form?: string | undefined;
--         formAction?: string | undefined;
--         formEncType?: string | undefined;
--         formMethod?: string | undefined;
--         formNoValidate?: boolean | undefined;
--         formTarget?: string | undefined;
--         height?: number | string | undefined;
--         list?: string | undefined;
--         max?: number | string | undefined;
--         maxLength?: number | undefined;
--         min?: number | string | undefined;
--         minLength?: number | undefined;
--         multiple?: boolean | undefined;
--         name?: string | undefined;
--         pattern?: string | undefined;
--         placeholder?: string | undefined;
--         readOnly?: boolean | undefined;
--         required?: boolean | undefined;
--         size?: number | undefined;
--         src?: string | undefined;
--         step?: number | string | undefined;
--         type?: HTMLInputTypeAttribute | undefined;
--         value?: string | ReadonlyArray<string> | number | undefined;
--         width?: number | string | undefined;
-- 
--         onChange?: ChangeEventHandler<T> | undefined;

type InputHTMLAttributes otherProps =
  HTMLAttributes +
    ( accept :: String
    , alt :: String
    , autoComplete :: String
    , autoFocus :: Boolean
    , capture :: Boolean
    , checked :: Boolean
    , crossOrigin :: String
    , disabled :: Boolean
    , enterKeyHint :: String
    , form :: String
    , formAction :: String
    , formEncType :: String
    , formMethod :: String
    , formNoValidate :: Boolean
    , formTarget :: String
    , height :: Number
    , list :: String
    , max :: Opt Number
    , maxLength :: Opt Number
    , min :: Opt Number
    , minLength :: Opt Number
    , multiple :: Boolean
    , onChange :: EventHandler
    , name :: String
    , pattern :: String
    , placeholder :: String
    , readOnly :: Boolean
    , required :: Boolean
    , size :: Number
    , src :: String
    , step :: Opt Number
    -- This collides with `Check` - take care of it upstream
    -- , type :: String
    , value :: String
    , width :: Number
    | otherProps
    )

type SelectHTMLAttributes otherProps =
  HTMLAttributes +
    ( autoComplete :: String
    , autoFocus :: Boolean
    , disabled :: Boolean
    , form :: String
    , multiple :: Boolean
    , name :: String
    , required :: Boolean
    , size :: Number
    , value :: String
    , onChange :: EventHandler
    | otherProps
    )
