{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2023 by Bernd Gabriel

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS, DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

{$ifdef FPC}

{ Identify FreePascal/Lazarus Compiler Version:

    const lcl_fullversion = ((lcl_major * 100 + lcl_minor) * 100 + lcl_release) * 100 + lcl_patch;

}

  {$define LCL}
  {$mode delphi}
  {$H+}

  {$define FPC_TODO}

  {$define MetaFileMissing}

  {$define HasStrictDelimiter}

  {.$define ver100_plus}
  {.$define ver120_plus}

  {$define TSearchRecHasNoTimestamp}

  { Compiler Features }

  {$ifdef LCLQt5}
    {$define OwnPaintPanelDoubleBuffering}
  {$endif}

{$else}

{ Identify Delphi Compiler Version:

  #   Compiler      Version Compiler Defined Symbol Codename
  D28 Delphi 11       35    VER350                  Alexandria
  D27 Delphi 10.4     34    VER340                  Sydney
  D26 Delphi 10.3     33    VER330                  Rio
  D25 Delphi 10.2     32    VER320                  Tokyo
  D24 Delphi 10.1     31    VER310                  Berlin
  D23 Delphi 10       30    VER300                  Seattle
  D22 Delphi XE8      29    VER290
  D21 Delphi XE7      28    VER280
  D20 Delphi XE6      27    VER270
  D19 Delphi XE5      26    VER260
  D18 Delphi XE4      25    VER250
  D17 Delphi XE3      24    VER240
  D16 Delphi XE2      23    VER230
  D15 Delphi XE       22    VER220
  D14 Delphi 2010     21    VER210
  D12 Delphi 2009     20    VER200

  unsupported Versions:

  D11 Delphi 2007     18.5  VER185
  D10 Delphi 2006     18    VER180
  D9  Delphi 2005     17    VER170
  D8  Delphi 8        16    VER160
  D7  Delphi 7        15    VER150
  D6  Delphi 6        14    VER140
      C++Builder 5          VER130 BCB
  D5  Delphi 5              VER130
      C++Builder 4          VER125
  D4  Delphi 4              VER120
      C++Builder 3          VER110
  D3  Delphi 3              VER100
      C++Builder 1          VER93
  D2  Delphi 2              VER90
  D1  Delphi 1              VER80

  Since Delphi 6 the version number can be found in the system constant System.CompilerVersion

  NOTICE: HtmlViewer 11.10 no longer supports versions before Delphi 2009

  A complete list of version numbers can be found at:

  https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Compiler_Versions
}

  {$define VCL}
  {$define Windows}
  {$ifdef CONDITIONALEXPRESSIONS} {Delphi 6 and up}
    {.$define ver100_plus}
    {.$define ver120_plus}
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
                                      {$define Compiler31_Plus}
                                      {$ifdef ver310}
                                      {$else}
                                        {$define Compiler32_Plus}
                                        {$ifdef ver320}
                                        {$else}
                                          {$define Compiler33_Plus}
                                          {$ifdef ver330}
                                          {$else}
                                            {$define Compiler34_Plus}
                                            {$ifdef ver340}
                                            {$else}
                                              {$define Compiler35_Plus}
                                              {$ifdef ver350}
                                              {$else}
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
    {$ifndef Compiler20_Plus}
      // Might still work or not. Oldest tested version is 2009.
      {$message 'Delphi versions before 2009 are no longer supported.'}
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
    {.$define UseGenerics} // you may use generics
  {$endif}

  {$ifdef Compiler21_Plus}
    {$define HasGestures}
  {$endif}

  {$ifdef Compiler22_Plus}
    {$define HasGDIPlus}
  {$else}
    {$define TSearchRecHasNoTimestamp}
  {$endif}

  {$ifdef Compiler23_Plus}
  {$else}
    {$define UseASMx86}
  {$endif}

  {$ifdef Compiler34_Plus}
    {$define UseGenerics} // you must use generics
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
{$define NoGDIPlus} {The gdiplus.dll provides Alpha transparency support for
        PNG images and provides support for TIFF images.
        If, for some reason the use of the gdiplus.dll is
        undesirable, define "NoGDIPlus" by removing the '.'.}

{.$define NoMetaFile} {Metafiles may be displayed by undefining this constant.
        NoMetaFile is defined by default to emphasize the fact that Metafile
        display and some printing operations are currently incompatible in Win98.
        For applications where printing is unimportant or Win98/95/ME need not
        be supported, undefining this constant will allow Metafiles to be
        displayed.}
{.$define BorderSinglePixelException} {Drawing 4 sided polygon the borders
  with single pixel thickness, the drawing can be done by lines by defining this
  constant. On print preview results are better without the single pixel exception,
  but it does not work correctly on certain printers, so the default behaviour
  can be overrules this way.}
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

{$ifndef OwnPaintPanelDoubleBuffering} // required for some compiler versions
{.$define OwnPaintPanelDoubleBuffering}
        {Issue 57: Delphi 7? or Russian? Locale? + XP-Manifest produce misplaced
        button captions, which I (BG) cannot reproduce with D6, D2007 or D2009.
        But SchwarzK...@yandex.ru reports, that this issue appears only since r118.
        This switch reactivates the manual double buffering in TPaintPanel.Paint(), which
        was removed with r118.
        Due to several redraw errors with Delphi, OwnPaintPanelDoubleBuffering now is always set for Delphi.}
{$endif}

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

{.$define UseHandCursor}
        {This uses the HtmlViewers Link Pointer instead of the System's crHandPoint}

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
{JPM_DEBUGGING define is used for internal debugging using CodeSite Express that is included with some
versions of RAD Studio since XE2.  This should never be used for release software and for component usage.}
  {$ifdef win32}
  {.$define JPM_DEBUGGING}
  {$endif}
  {$define has_StyleElements}
{$endif}

{
Using Third Party Unicode Controls

Unfortunately, the VCL controls that come with Delphi before 2009 do not handle
Unicode well.  In fact, in many cases, they do not work well with multibyte fonts
either.  For this reason, provision had been made to use either the TNT third
party controls.

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


