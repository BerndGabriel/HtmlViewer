--------------------------------------------------------------------------------
Welcome to GitHub!
--------------------------------------------------------------------------------

Developers might want to use Tortoise-GIT or "Git Gui" to keep their sources up-to-date.

Or even become a member of GitHub, clone the repository and push their changes 
to their clone. If they are of public interest you can request me to pull it 
into my branch where it will become part of the "official" HtmlViewer.

--------------------------------------------------------------------------------
HtmlViewer 11
--------------------------------------------------------------------------------

The current version is HtmlViewer 11.10 in the default branch "HtmlViewer-11.10".
The next version is growing up in branch "master". (You're currently viewing "master" branch.)

The HtmlViewer component set consists of the THtmlViewer, TFrameViewer,
and TFrameBrowser components. All three are HTML document display components:

THtmlViewer
- The basic component.
- THtmlViewer displays single (non-frame) documents from file systems and resources.
- It also forms the basis for the other two components.

TFrameViewer (deprecated)
- Displays both frame and single HTML documents.
- TFrameViewer is oriented more for local file system use.

TFrameBrowser
- Also displays frame and single HTML documents.
- TFrameBrower is oriented for use with local file systems and toward Internet
  style protocols and URL usage.
- Additional code and/or components are generally required to get data from other
  sources than the local file system. We recommend using Indy or ICS components.

These components support most of the HTML 4.01 specifications 
with many additional popular HTML 5 enhancements.
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
- Can print multiple pages horizontally
- Print preview
- Text search
- Copy to clipboard
- Subscripts and superscripts
- Unicode, UTF-8 and many single and multi byte character codes supported
- DPI scaling

While HTML documents are normally associated with the Internet, they are also
very useful for displaying all kinds of textual material such as documentation,
helpfiles, etc. Graphics are easily incorporated in these documents.

For a detailed list and demonstration of features start FrameDem.exe.

For a detailed list of changes and bugfixes per version start FrameDem.exe 
and click "What's new" or see "./Demos/Compiled Framedemo/whatsnew.htm".

--------------------------------------------------------------------------------

Which HtmlViewer version should I use?

HtmlViewer 11.x (recommended)
- is a full UnicodeString version suitable for projects 
  with most common codepages/charsets.
- with Delphi 6..2007 requires the TntUnicodeControls.
- works with Delphi  6      or newer (recommended at least 2009)
- works with Lazarus 0.9.30 or newer (recommended at least 1.2.x)
  for Windows, Linux and Raspbian
- 32 and 64 Bit

HtmlViewer 11.x is available at: https://sourceforge.net/projects/htmlviewer/
HtmlViewer development is hosted at: https://github.com/BerndGabriel/HtmlViewer

If you are using HtmlViewer 11 with Delphi 6..2007 you will need the
TntUnicodeControls 2.3.0 or 2.2.1, which are available at:

http://www.yunqa.de/delphi/doku.php/products/tntunicodecontrols/index

Please install them into your IDE.
--------------------------------------------------------

Copyright (c) 1995 - 2008 by L. David Baldwin
Copyright (c) 1995 - 2008 by Anders Melander (DitherUnit.pas)
Copyright (c) 1995 - 2008 by Ron Collins (HtmlGif1.pas)
Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)
Copyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)
Copyright (c) 2009 - 2022 by HtmlViewer Team 

See included License.txt
--------------------------------------------------------------------------------