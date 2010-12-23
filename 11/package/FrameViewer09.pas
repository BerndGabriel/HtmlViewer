{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit FrameViewer09; 

interface

uses
    HtmlGlobals, Htmlsbs1, HTMLSubs, HTMLUn2, Htmlview, ReadHTML, StylePars, StyleUn, URLSubs, DitherUnit, FramBrwz, FrameViewerReg, FramView, GDIPL2A, HtmlGif1, HTMLGif2, HtmlBuffer, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FrameViewerReg', @FrameViewerReg.Register); 
end; 

initialization
  RegisterPackage('FrameViewer09', @Register); 
end.
