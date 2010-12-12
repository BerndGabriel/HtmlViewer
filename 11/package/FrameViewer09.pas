{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden ! 
  Dieser Quelltext dient nur dem Ã œ bersetzen und Installieren des Packages.
 }

unit FrameViewer09; 

interface

uses
  HtmlGlobals, Htmlsbs1, HTMLSubs, HTMLUn2, Htmlview, ReadHTML, StylePars, StyleUn, URLSubs, DitherUnit, FramBrwz, FrameViewerReg, FramView, GDIPL2A, HtmlGif1, HTMLGif2, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FrameViewerReg', @FrameViewerReg.Register); 
end; 

initialization
  RegisterPackage('FrameViewer09', @Register); 
end.
