{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FrameViewer09; 

interface

uses
    DitherUnit, FramBrwz, FrameViewerReg, FramView, HtmlBuffer, HtmlGif1, 
  HTMLGif2, HtmlGlobals, HtmlMisc, Htmlsbs1, HTMLSubs, HTMLUn2, HtmlView, 
  ReadHTML, StylePars, StyleUn, URLSubs, msimg32, AlphaBlendUn, HSLUtils, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FrameViewerReg', @FrameViewerReg.Register); 
end; 

initialization
  RegisterPackage('FrameViewer09', @Register); 
end.
