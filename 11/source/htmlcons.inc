{Version 10.2}

{$ifdef FPC}

{ Identify FreePascal/Lazarus Compiler Version }

  {$define LCL}
  {$MODE Delphi}

  {$define FPC_TODO}
  {$define FPC_TODO_PRINTING}

  {$define NoGDIPlus}

  {$define CopyPaletteMissing}
  {$define TransparentStretchBltMissing}
  {$define MetaFileMissing}
  {$define UnitConstsMissing}

  {$define ver100_plus}
  {$define ver120_plus}
  {$define Delphi6_Plus}
{$else}

{ Identify Delphi Compiler Version:

  Compiler    Version Compiler Defined Symbol
  Delphi XE     22    VER220
  Delphi 2010   21    VER210
  Delphi 2009   20    VER200
  Delphi 2007   18.5  VER185
  Delphi 2006   18    VER180
  Delphi 2005   17    VER170
  Delphi 8      16    VER160
  Delphi 7      15    VER150
  Delphi 6      14    VER140
  C++Builder 5        VER130 BCB
  Delphi 5            VER130
  C++Builder 4        VER125
  Delphi 4            VER120
  C++Builder 3        VER110
  Delphi 3            VER100
  C++Builder 1        VER93
  Delphi 2            VER90
  Delphi 1            VER80

  Since Delphi 6 the version number can be found in the system constant System.CompilerVersion

  NOTICE: HtmlViewer no longer supports versions before Delphi 6
  BG, 20.11.2010: HtmlViewer sources use neither ver100_plus nor ver120_plus any more.
  We keep the defines alive for custom code which might still use them.
}

  {$define VCL}
  {$ifdef CONDITIONALEXPRESSIONS} {Delphi 6 and up}
    {$define ver100_plus}
    {$define ver120_plus}
    {$define Delphi6_Plus}
    {$ifdef ver140}
    {$else}
      {$define Delphi7_Plus}
      {$ifdef ver140}
      {$else}
        {$ifdef ver140}
        {$else}
          {$ifdef ver150}
          {$else}
            {$ifdef ver160}
            {$else}
              {$define Compiler17_Plus}
              {$ifdef ver170}
              {$else}
                {$ifdef ver180}
                {$else}
                  {$ifdef ver185}
                  {$else}
                    {$define Compiler20_Plus}
                  {$endif}
                {$endif}
              {$endif}
            {$endif}
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$else}
    {$ifdef ver80}  {Delphi 1}
    {$else}
      {$ifdef ver90}  {Delphi 2}
      {$else}
        {$ifdef ver93}  {C++Builder 1}
          {$Define CppBuilder}
        {$else}
          {$define ver100_plus}
          {$ifdef ver100} {Delphi 3}
            {$define D3_BCB3}
          {$else}
            {$ifdef ver110} {C++Builder 3}
              {$define D3_BCB3}
              {$Define CppBuilder}
              {$ObjExportAll On}
            {$else}
              {$define ver120_plus}
              {$ifdef Ver120} {Delphi 4}
              {$else}
                {$ifdef ver125} {C++Builder 4}
                  {$Define CppBuilder}
                  {$ObjExportAll On}
                {$else}
                  {$ifdef Ver130} {Delphi 5, C++Builder 5}
                    {$ifdef BCB}   {C++Builder}
                      {$Define CppBuilder}
                      {$ObjExportAll On}
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

  {$ifndef ver100_plus}
    {$define CopyPaletteMissing}
    {$define TransparentStretchBltMissing}
  {$endif}

  {$ifdef Compiler17_Plus}
    {$define UseInline}
  {$endif}

  { Set Delphi Compiler Switches }

  {$ifdef ver150}  {Delphi 7}
  {$WARN Unsafe_Type off}
  {$WARN Unsafe_Code off}
  {$WARN Unsafe_Cast off}
  {$endif}

  {$A+,B-,F-,G+,I+,P+,T-,V+,X+,R-}

  {$ifdef Win32}
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

  {$ifdef Delphi6_Plus}
    {$Warn Symbol_Platform Off}
  {$endif}

{$endif}

{ Define Aspects Of HtmlViewer }

{.$Define NoGDIPlus} {The gdiplus.dll provides Alpha transparency support for
        PNG images.  If, for some reason the use of the gdiplus.dll is
        undesirable, define "NoGDIPlus" by removing the '.'.
        Defining NoGDIPlus will remove support for PNG images entirely.}

{$Define NoMetaFile} {Metafiles may be displayed by undefining this constant.
        NoMetaFile is defined by default to emphasize the fact that Metafile
        display and some printing operations are currently incompatible in Win98.
        For applications where printing is unimportant or Win98/95/ME need not
        be supported, undefining this constant will allow Metafiles to be
        displayed.}

{$Define NoTabLink}  {To eliminate tabbing between links, define "NoTabLink" by
        removing the '.'.  For documents with hundreds of links, tabbing may not
        be desirable and also may cause delays in loading.  Even when NoTabLink
        is not defined, there is another constant, MaxTab, which eliminates
        tabbing to MaxTab links.  MaxTab is defined in readhtml.pas}

{$Define FastRadio}
       {if you have a form with a great many radio buttons, you may want to
        define FastRadio to help speed the action.  Note that this option
        changes the way tabbing works.  With FastRadio off, tabs move only to
        the next control that is visible.  With FastRadio on, tabs move to
        the next control even if it is not visible.}

{.$Define Quirk}
        {Defining Quirk makes changes which simulate some of the
        quirks of other browsers.  It may be desirable to define this constant
        if you expect to be displaying HTML which might depend on these quirks.
        Currently, Quirk forces the font color and size to return to default
        values in each table cell.}

{.$Define OpOnChange}
        {Without this definition, the <select> form controls (TListbox and
        TCombobox) will issue the OnObjectChange event only when the control
        loses the focus which is according to HTML specs.  Defining OpOnChange
        simulates the way IExplorer does it, issuing the OnChange event
        immediately when a change is made.}

{.$Define UseTNT}
        {Defining UseTNT allows the TNT Unicode controls to be used for HTML Form
        controls in place of the Delphi TEdit, TButton, TListbox, TCombobox, and
        TMemo controls.  See "Using Third Party Controls" below.}

{.$Define UseElpack}
        {Defining UseElpack allows the Elpack Unicode controls to be used for
        HTML Form controls in place of the Delphi TEdit, TButton, TListbox,
        TCombobox, and TMemo controls.  See "Using Third Party Controls" below.}
{
Using Third Party Unicode Controls

Unfortunately, the VCL controls that come with Delphi do not handle Unicode well.
In fact, in many cases, they do not work well with multibyte fonts either.  For
this reason, provision had been made to use either the TNT or Elpack third party
controls.

The TNT controls may be obtained at
http://home.ccci.org/wolbrink/tnt/delphi_unicode_controls.htm.  They are
freeware.  They handle Unicode well in Win2000 and WinXP (probably NT also).
However, with Win95/98 they are not Unicode but do handle multibyte and other
character sets well with the exception of UTF-8.  The TNT controls do not work
with Delphi 4.

The Elpack controls may be purchased at http://www.eldos.org/elpack/elpack.html.
For use with the HTML Components, the item you will want is the "LMD Elpack 4.0
Standard VCL".  The Elpack controls are fully Unicode for Win95 through WinXP.

Either of these control sets may be used by defining the appropriate compile
time constant, "UseTNT" or "UseElpack".  You can define these in htmlcons.inc or
in the Project|Options dialog, Directories/Conditionals tab.  Be sure and do a
complete Build on your application after changing these compile time constants.

HtmlGlobals.pas defines several new symbols to represent the form controls and types
depending on the set of controls being used. These types are
ThtEdit, ThtButton, ThtMemo, ThtCombobox, ThtListbox, ThtString and ThtStringList.

The definitions of these symbols change with the set of controls being used.  So,
for instance, ThtEdit is the same as TEdit for the Delphi controls but becomes
TTntEdit when UseTNT is defined or TElEdit when UseElpack is defined. And ThtString
is string for Delphi controls and WideString for the unicode controls.

If your application accesses the form controls for special purposes, you should
use these new symbols.  For example code like,

      if TFormControlObj(Items[I]).TheControl is TEdit then
        TEdit(TFormControlObj(Items[I]).TheControl).Text := ........

should become:

      if TFormControlObj(Items[I]).TheControl is ThtEdit then
        ThtEdit(TFormControlObj(Items[I]).TheControl).Text := ........
}
{
  At last decide, which kind of strings to use:
  if "UseUnicode" is defined
    WideChars and WideStrings are used.
  Otherwize
    AnsiChars and AnsiString are used.

  HtmlGlobals defines types Tht* according to these settings.
}
{$ifdef UseTNT}
  {$define UseUnicode}
{$endif}
{$ifdef UseElpack}
  {$define UseUnicode}
{$endif}
{$ifdef UNICODE}
  {$define UseUnicode}
{$endif}

{$ifdef UseUnicode}
{$else}
{$endif}

{$ifdef UseUnicode}
  {$ifdef UNICODE}
  {$else}
  {$endif}
{$else}
  {$ifdef UNICODE}
  {$else}
  {$endif}
{$endif}

