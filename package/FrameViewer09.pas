{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FrameViewer09; 

interface

uses
    HtmlGlobals, Htmlsbs1, HTMLSubs, HTMLUn2, HtmlView, ReadHTML, StylePars, StyleUn, URLSubs, DitherUnit, FramBrwz, FrameViewerReg, FramView, HtmlGif1, HTMLGif2, HtmlBuffer, HtmlMisc, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FrameViewerReg', @FrameViewerReg.Register); 
end; 

initialization
  RegisterPackage('FrameViewer09', @Register); 
end.
