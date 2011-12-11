--------------------------------------------------------------------------------
HtmlViewer 10.2 and 11
--------------------------------------------------------------------------------

What are the HtmlViewer Components?

The HtmlViewer component set consists of the THtmlViewer, TFrameViewer,
and TFrameBrowser components. All three are HTML document display components:

THtmlViewer
- The basic component.
- THtmlViewer displays single (non-frame) documents.
- It also forms the basis for the other two components.

TFrameViewer (deprecated)
- Displays both frame and single HTML documents.
- TFrameViewer is oriented more for local file system use.

TFrameBrowser
- Also displays frame and single HTML documents.
- TFrameBrower is oriented for use with local file systems and toward Internet
  style protocols and URL usage.
- Additional code and/or components are generally required to get data from
  other sources than the local file system.

These components support most of the HTML 3.2 specifications with many
additional popular HTML 4 enhancements.
Many Cascading Style Sheet properties are also supported.

Some features:

- Cascading Stylesheets
- Large HTML files
- HTML Frames
- HTML Forms
- HTML Tables
- Bitmap, GIF, JPEG, and PNG Images
- Transparent images
- Image caching
- Left and right floating images
- Image sizing attributes
- Client side image maps
- Background colors and images
- Font sizes, styles, and colors with HTML tags or default settings
- Formatted printing of the HTML document
- Print preview
- Text search
- Copy to clipboard
- Subscripts and superscripts

While HTML documents are normally associated with the Internet, they are also
very useful for displaying all kinds of textual material such as documentation,
helpfiles, etc. Graphics are easily incorporated in these documents.

For a detailed list and demonstration of features start FrameDem.exe.

For a detailed list of changes and bugfixes per version start FrameDem.exe 
and click "What's new" or see "./Demos/Compiled Framedemo/whatsnew.htm".
--------------------------------------------------------------------------------

Which HtmlViewer version should I use?

HtmlViewer 10.2
- is a mixed AnsiString/WideString version suitable for projects 
  with a single codepage/charset.

HtmlViewer 11
- is a full WideString version suitable for projects
  with several codepages/charsets.
- with Delphi 6..2007 requires the TntUnicodeControls.
- works with Lazarus 0.9.31

--------------------------------------------------------------------------------

HtmlViewer 10.2 and 11 are available at: http://code.google.com/p/thtmlviewer/
HtmlViewer developement is hosted at: https://github.com/BerndGabriel/HtmlViewer

If you are using HtmlViewer 11 with Delphi 6..2007 you will need the
TntUnicodeControls 2.3.0 or 2.2.1, which are available at:

http://www.yunqa.de/delphi/doku.php/products/tntunicodecontrols/index

Please install them into your IDE.

--------------------------------------------------------------------------------

Copyright (c) 1995 - 2008 by L. David Baldwin
Copyright (c) 1995 - 2008 by Anders Melander (DitherUnit.pas)
Copyright (c) 1995 - 2008 by Ron Collins (HtmlGif1.pas)
Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)
Copyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)
Copyright (c) 2009 - 2012 by Bernd Gabriel (Fixes, Enhancements)

See included License.txt

--------------------------------------------------------------------------------

