{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FrameViewer09;

interface

uses
  DitherUnit, FramBrwz, FrameViewerReg, FramView, HtmlBuffer, HtmlGif1, 
  HTMLGif2, HtmlGlobals, HtmlMisc, Htmlsbs1, HTMLSubs, HTMLUn2, HtmlView, 
  ReadHTML, StylePars, StyleUn, URLSubs, msimg32, AlphaBlendUn, HSLUtils, 
  BuffConv, BuffConvArrays, HtmlSymb, BegaHtmlPrintPreviewForm, 
  BegaMetaFilePrinter, BegaPreview, BegaPreviewForm, BegaPreviewPanel, 
  BegaScrollBox, BegaZoom, GDIPL2A, HtmlCaches, HtmlFonts, HtmlImages, 
  MetaFilePrinter, StyleTypes, vwPrint, WideStringsLcl, UrlConn, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FrameViewerReg', @FrameViewerReg.Register);
end;

initialization
  RegisterPackage('FrameViewer09', @Register);
end.
