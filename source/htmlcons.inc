{Version 11}
//{$IFDEF CONDITIONALEXPRESSIONS}
//
//
//  // Must be at the top...
//  {$IF CompilerVersion >= 24.0}
//    {$LEGACYIFEND ON}
//  {$IFEND}
//{$ENDIF}
{$ifdef FPC}

{ Identify FreePascal/Lazarus Compiler Version }

  {$define LCL}
  {$mode delphi}
  {$H+}

  {$define FPC_TODO}

  {$define MetaFileMissing}

  {$define HasStrictDelimiter}

  {$define ver100_plus}
  {$define ver120_plus}

{$else}

{ Identify Delphi Compiler Version:

  #   Compiler    Version Compiler Defined Symbol
  D23 Delphi 10 Seattle 30  VER300
  D22 Delphi XE8    29    VER290
  D21 Delphi XE7    28    VER280
  D20 Delphi XE6    27    VER270
  D19 Delphi XE5    26    VER260
  D18 Delphi XE4    25    VER250
  D17 Delphi XE3    24    VER240
  D16 Delphi XE2    23    VER230
  D15 Delphi XE     22    VER220
  D14 Delphi 2010   21    VER210
  D12 Delphi 2009   20    VER200
  D11 Delphi 2007   18.5  VER185
  D10 Delphi 2006   18    VER180
  D9  Delphi 2005   17    VER170
  D8  Delphi 8      16    VER160
  D7  Delphi 7      15    VER150
  D6  Delphi 6      14    VER140
      C++Builder 5        VER130 BCB
  D5  Delphi 5            VER130
      C++Builder 4        VER125
  D4  Delphi 4            VER120
      C++Builder 3        VER110
  D3  Delphi 3            VER100
      C++Builder 1        VER93
  D2  Delphi 2            VER90
  D1  Delphi 1            VER80

  Since Delphi 6 the version number can be found in the system constant System.CompilerVersion

  NOTICE: HtmlViewer no longer supports versions before Delphi 6
  BG, 20.11.2010: HtmlViewer sources use neither ver100_plus nor ver120_plus any more.
  We keep the defines alive for custom code which might still use them.

  A complete list of version numbers can be found at:

  http://docwiki.embarcadero.com/RADStudio/XE8/de/Compiler-Versionen
}

  {$define VCL}
  {$define Windows}
  {$ifdef CONDITIONALEXPRESSIONS} {Delphi 6 and up}
    {$define ver100_plus}
    {$define ver120_plus}
    {$ifdef ver140}
    {$else}
      {$define Delphi7_Plus}
      {$define Compiler15_Plus}
      {$ifdef ver150}
      {$else}
        {$define Compiler16_Plus}
        {$ifdef ver160}
        {$else}
          {$define Compiler17_Plus}
          {$ifdef ver170}
          {$else}
            {$define Compiler18_Plus}
            {$ifdef ver180}
            {$else}
              {$define Compiler19_Plus}
              {$ifdef ver185}
              {$else}
                {$define Compiler20_Plus}
                {$ifdef ver200}
                {$else}
                   {$define Compiler21_Plus}
                  {$ifdef ver210}
                  {$else}
                    {$define Compiler22_Plus}
                    {$ifdef ver220}
                    {$else}
                      {$define Compiler23_Plus}
                      {$ifdef ver230}
                      {$else}
                        {$define Compiler24_Plus}
                        {$ifdef ver240}
                        {$else}
                          {$define Compiler25_Plus}
                          {$ifdef ver250}
                          {$else}
                            {$define Compiler26_Plus}
                            {$ifdef ver260}
                            {$else}
                              {$define Compiler27_Plus}
                             {$ifdef ver270}
                             {$else}
                               {$define Compiler28_Plus}
                               {$ifdef ver280}
                               {$else}
                                 {$define Compiler29_Plus}
                                 {$ifdef ver290}
                                 {$else}
                                   {$define Compiler30_Plus}
                                   {$ifdef ver300}
                                   {$else}
                                   {$endif}
                                 {$endif}
                               {$endif}
                             {$endif}
                            {$endif}
                          {$endif}
                        {$endif}
                        {$LEGACYIFEND ON}
                      {$endif}
                    {$endif}
                  {$endif}
                {$endif}
              {$endif}
            {$endif}
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$else}
    {$message 'Sorry, but Delphi versions before 6 are not supported.'}
  {$endif}

  { Compiler Features }

  {$ifdef Compiler17_Plus}
    {$ifdef DEBUG}
    {$else}
      {$define UseInline}
    {$endif}
  {$endif}

  {$ifdef Compiler18_Plus}
    {$define HasStrictDelimiter}
  {$endif}

  {$ifdef Compiler20_Plus}
    {$define TEditHasTextHint}
    {$define TComboBoxHasTextHint}
  {$endif}

  {$ifdef Compiler21_Plus}
    {$define HasGestures}
  {$endif}

  {$ifdef Compiler22_Plus}
    {$define HasGDIPlus}
  {$endif}

  {$ifdef Compiler23_Plus}
  {$else}
    {$define UseASMx86}
  {$endif}

  { Set Delphi Compiler Switches }

  {$ifdef ver150}  {Delphi 7}
  {$warn Unsafe_Type off}
  {$warn Unsafe_Code off}
  {$warn Unsafe_Cast off}
  {$endif}

  {$A+,B-,F-,G+,I+,P+,T-,V+,X+,R-}

  {$ifdef Win32}
    {$J+}  {typed constants are modifiable}
    {$H+}  {LongStrings On}
  {$endif}
  {$ifdef Win64}
    {$J+}  {typed constants are modifiable}
    {$H+}  {LongStrings On}
  {$endif}

  {$ifndef DebugIt}
    {$W-}  {Stack frames off}
    {$Q-}  {overflow checking off}
    {$S-}  {stack checking off}
    {$C-}  {Assertions off}
    {$ifdef Win32}
      {$O+}  {optimization on}
    {$endif}
  {$else}
    {$W+}  {Stack frames on}
    {$Q+}  {overflow checking on}
    {$S+}  {stack checking on}
    {$C+}  {Assertions on}
    {$ifdef Win32}
      {$O-}  {optimization off}
    {$endif}
  {$endif}

  {$warn Symbol_Platform Off}

  {$IFDEF UNICODE}
    {$HIGHCHARUNICODE ON}  {make all #nn char constants Wide for better portability}
  {$ENDIF}

  {$define OwnPaintPanelDoubleBuffering}

{$endif}

{ Define Aspects Of HtmlViewer }

{$ifdef MSWindows}
{.$define NoGDIPlus} {The gdiplus.dll provides Alpha transparency support for
        PNG images and provides support for TIFF images.
        If, for some reason the use of the gdiplus.dll is
        undesirable, define "NoGDIPlus" by removing the '.'.
        Defining NoGDIPlus will remove support for PNG images entirely.}

{.$define NoMetaFile} {Metafiles may be displayed by undefining this constant.
        NoMetaFile is defined by default to emphasize the fact that Metafile
        display and some printing operations are currently incompatible in Win98.
        For applications where printing is unimportant or Win98/95/ME need not
        be supported, undefining this constant will allow Metafiles to be
        displayed.}
{$else}
  {$define NoGDIPlus}
  {$define NoMetaFile}
  {$define NoFlatScrollbars}
  {$define MultiMediaMissing}
{$endif}

{.$define NoTabLink}  {To eliminate tabbing between links, define "NoTabLink" by
        removing the '.'.  For documents with hundreds of links, tabbing may not
        be desirable and also may cause delays in loading.  Even when NoTabLink
        is not defined, there is another constant, MaxTab, which eliminates
        tabbing to MaxTab links.  MaxTab is defined in readhtml.pas}

{$define FastRadio}
       {if you have a form with a great many radio buttons, you may want to
        define FastRadio to help speed the action.  Note that this option
        changes the way tabbing works.  With FastRadio off, tabs move only to
        the next control that is visible.  With FastRadio on, tabs move to
        the next control even if it is not visible.}

{.$define OpOnChange}
        {Without this definition, the <select> form controls (TListbox and
        TCombobox) will issue the OnObjectChange event only when the control
        loses the focus which is according to HTML specs.  Defining OpOnChange
        simulates the way IExplorer does it, issuing the OnChange event
        immediately when a change is made.}

{.$define OwnPaintPanelDoubleBuffering}
        {Issue 57: Delphi 7? or Russian? Locale? + XP-Manifest produce misplaced
        button captions, which I (BG) cannot reproduce with D6, D2007 or D2009.
        But SchwarzK...@yandex.ru reports, that this issue appears only since r118.
        This switch reactivates the manual double buffering in TPaintPanel.Paint(), which
        was removed with r118.
        Due to several redraw errors with Delphi, OwnPaintPanelDoubleBuffering now is always set for Delphi.}

{$define HalfToneStretching}
        {This makes use of a better stretching methode provided by Windows: HALFTONE.
        So resized graphics (especially smaller ones) look MUCH better}

{.$define UseGlobalObjectId}
        {This gives each HTML element an application wide unique ID even if there
        is no ID or NAME attribute given in the HTML document. The unique ID is
        composed of 'GOID-' and a consecutive number starting with 1}

{.define UseOldStyleTypes}
        {Style types have been renamed to Tht.. to meet the Delphi style guide that types should start with a 'T'.
        As long as you did not change your code accordingly you can define this symbol.}

{$define EmptyCellsDefaultIsShow} {The early HtmlViewer was built with the
        default 'hide', but CSS 2.1 defines 'show' as default. A lot of page
        designs for use in HtmlViewer (including our table demo page) now use
        'hide' as default.
        New users should define EmptyCellsDefaultIsShow for CSS compliance.}

{.$define UseDeprecatedLoadMethods}
        {Brings back deprecated LoadStrings(), LoadImage(), LoadText*()..., ... methods.
        Prefer using the suggested replacement methods over defining this switch.}

{$ifdef Compiler20_Plus}
{$else}
  {TNT Unicode controls are mandatory for Delphi 6-2007}
  {$ifdef LCL}
  {$else}
    {$define UseTNT}
  {$endif}
{$endif}

{$ifdef Compiler23_Plus}
   {$define UseVCLStyles}
{$endif}

{$ifdef Compiler24_Plus}
  {$define HasSystemUITypes}
{JPM_DEBUGGING define is used for internal debugging using CodeSite Express that is included with
some versions of RAD Studio XE2.  This should never be used for release software and for component usage.}
  {$ifdef win32}
  {.$define JPM_DEBUGGING}
  {$endif}
   {$define has_StyleElements}
{$endif}

{
Using Third Party Unicode Controls

Unfortunately, the VCL controls that come with Delphi do not handle Unicode well.
In fact, in many cases, they do not work well with multibyte fonts either.  For
this reason, provision had been made to use either the TNT or Elpack third party
controls.

The TNT controls may be obtained at
http://www.yunqa.de/delphi/doku.php/products/tntunicodecontrols/index.
They are freeware.  They handle Unicode well in Win2000 and WinXP (probably NT
also). However, with Win95/98 they are not Unicode but do handle multibyte and
other character sets well with the exception of UTF-8.  The TNT controls do not
work with Delphi 4.

HtmlGlobals.pas defines several new symbols to represent the unicode char and string
types depending on the compilers native types and defines form controls and types
depending on the set of controls being used.  These types are ThtChar, ThtString,
ThtEdit, ThtButton, ThtMemo, ThtCombobox, ThtListbox, ThtString and ThtStringList.

The definitions of these symbols change with the compiler and/or set of controls
being used.  So, for instance, ThtEdit is the same as TEdit for the Delphi controls
but becomes TTntEdit when UseTNT is defined.  And ThtString is WideString for Delphi
6 .. 2007 and string since Delphi 2009.

If your application accesses the form controls for special purposes, you should
use these new symbols.  For example code like,

      if TFormControlObj(Items[I]).TheControl is TEdit then
        TEdit(TFormControlObj(Items[I]).TheControl).Text := ........

should become:

      if TFormControlObj(Items[I]).TheControl is ThtEdit then
        ThtEdit(TFormControlObj(Items[I]).TheControl).Text := ........
}


