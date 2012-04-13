{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BegaHtmlViewer09; 

interface

uses
  URLSubs, HtmlBuffer, HtmlDocument, HtmlGlobals, HtmlParser, HtmlStyles, HtmlSymbols, Parser, StyleParser, HtmlMisc, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('BegaHtmlViewer09', @Register); 
end.
