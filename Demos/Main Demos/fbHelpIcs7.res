        ��  ��                  #  0   H T M L   P A G E 0         0         <title>Demo Program Information</title>
<frameset  cols="170,*" border="0">
    <frame name="LeftWin" src="XLeft1.htm" marginwidth="10" marginheight="10" scrolling="no"  noresize>
    <frame name="RightWin" src="Page1.htm" marginwidth="5" marginheight="0" scrolling="auto">
</frameset>
 �  0   H T M L   P A G E 1         0         <html>
<head>
    <title>TFrameBrowser Demo - Internet Component Suite - ICS v7/v8</title>
    <link rel="StyleSheet" type="text/css" href="style.css">
    <style>
        body.indent {margin-left: 20px;}
    </style>
</head>

<body bgcolor="white" link="blue">   <!-- #3737a5-->

<p class="mainhead"><a name="Top">TFrameBrowser Demo</a><br>
    <small><small>Internet Component Suite - ICS v7/v8</small></small></p>
<tr>
  <td width="2%"></td>
  <td valign="top">

  <FONT FACE="Arial" >

  <p class="heading"><nobr>Demo Quick Facts -- Please Read</nobr></p>
    <ul>
      <li>For online browsing, enter the URL. For example:        
        <p style="margin-left:10pt">
        <a href="http://www.pbear.com/" target="_blank"><code>www.pbear.com/</code></a>
        or
        <a href="https://github.com/BerndGabriel/HtmlViewer/" target="_blank"><code>github.com/BerndGabriel/HtmlViewer</code></a>
        <p>
      <li>To view a local html or image file, use the <B>File</B> menu or enter
        <code>file:///</code> followed by the filename.<br>
        <br>
      <li>&nbsp;This document is a resource contained within this program. To
        view this document again in the future, select <b>Help|Demo Information</b>
        from the menu or enter <a href="res:///page0.htm" target="_top"><code>res:///page0.htm</code></a>.
        <br>
        <br>
      <li>To compile this demo program for your own needs, see <a href="Page3.htm"
          target="RightWin">Compiling the Demo</a>. <br>
        <br>
      <li>The latest HtmlViewer is available at: <a href="https://github.com/BerndGabriel/HtmlViewer">https://github.com/BerndGabriel/HtmlViewer</a><br>
        <br>
      <li>ICS v7/v8 and the SSL files may be downloaded from <a href="http://wiki.overbyte.be/wiki/index.php/ICS_Download">
        http://wiki.overbyte.be/wiki/index.php/ICS_Download </a>
    </ul>
    <p>


  <p class="heading"><nobr><a name="Overview">Overview</a></nobr></p>


  <p>The HTML Component set consists of the <b>THtmlViewer</b>, <b>TFrameViewer</b>, and <b>TFrameBrowser</b>
  components.   All three are HTML document display components:</p>

  <table width="100%" cellspacing="2" cellpadding="2" border="0">
  <tr>
      <th width="2%"></th>
      <th class="dorange" valign="top" align="left">THtmlViewer</th>
      <td valign="top">The basic component.  <b>THtmlViewer</b> displays single (non-frame)
                     documents.  It forms the basis for the other two components.<br><br></td>
  </tr>
  <tr>
      <td></td>
      <th class="dorange" valign="top" align="left">TFrameViewer</th>
      <td valign="top">Displays both Frame and single HTML documents.  <b>TFrameViewer</b> is
                     oriented more for local disk file use.<br><br></td>
  </tr>
  <tr>
      <th></th>
      <th class="dorange" valign="top" align="left">TFrameBrowser&nbsp;&nbsp;&nbsp;</th>
      <td valign="top">Also displays Frame and single HTML documents.  However, <b>TFrameBrower</b> is
                     oriented toward Internet style protocols and URL usage.  Additional
                     code and/or components are generally required to use <b>TFrameBrowser</b>.<br><br></td>
  </tr>
  </table>

  <p class="heading"><nobr><a name="Demos">Demo Program</a></nobr></p>

  <p>The <B>FrameBrowserId</B> demo program illustrates how the <b>TFrameBrowser</b>
  component might be used in a complete application.<p>The demo program supports the following protocols:</p>

  <table width="100%" cellspacing="2" cellpadding="2" border="0">
  <tr>  <tr>
      <th width="2%"></th>
      <th valign="top" align="left">http&nbsp;&nbsp;&nbsp;</th>
      <td valign="top">Internet online protocol<br></td>
  </tr>

      <th width="2%"></th>
      <th valign="top" align="left">https&nbsp;&nbsp;&nbsp;</th>
      <td valign="top">Internet online SSL protocol (optional)<br></td>
  </tr>
  <tr>
      <th width="2%"></th>
      <th valign="top" align="left">file&nbsp;&nbsp;&nbsp;</th>
      <td valign="top">Internet local file protocol<br></td>
  </tr>
  <tr>
      <th width="2%"></th>
      <th valign="top" align="left">res&nbsp;&nbsp;&nbsp;</th>
      <td valign="top">Specail protocol to read HTML documents from an application's resources<br></td>
  </tr>
  </table>


  <p class="heading"><nobr><a name="Protocols">Protocol Details</a></nobr></p>

  <p>When initially loading an HTML document, the full URL including the protocol should be used.
  From that point on, TFrameBrowser is capable of adding the protocol and path to partial URLs found in
  the document.</p>

  <p>The <b>http</b>, <b>https</b>, and <b>file</b> protocols supported by the demo program are standard.
  &nbsp;URLs using these protocols are in the form:</p>

  <pre style="margin-left:10pt">
  http://www.pbear.com/index.html
  file:///c|/thtml4/framedem.htm</pre>

  <p>The <b>res</b> protocol may be used to access HTML documents and their images stored in a program's
  resources.  URLs with the <b>res</b> protocol have the form:</p>

  <pre style="margin-left:10pt">
  res:///helpinfo.html</pre>

  <p>The <b>res</b> protocol supports document extensions of <code>HTM, HTML, GIF, JPG, JPEG,
  PNG, BMP, CSS,</code> and <code>TXT</code>. See <a href="page5.htm#Resources" target="RightWin">Adding HTML Documents
  to an Application's Resources</a> for
  information on how to add HTML resources to an application.</p>

  <p>Additional special protocols may be easily added by the programmer.  Protocols might be defined for reading
  encrypted files or reading HTML from a database, for instance.  See <a href="page5.htm#AddingProtocols">Adding Your Own Protocols</a>.
  </p>
  </font>

  </td>
  <td width="2%"></td>
</tr>
</table>
<p>&nbsp;</p>
</body>
</html>
P  0   H T M L   P A G E 3         0         <html>
<head>
	<title>Compiling the Demo Program</title>
    <link rel="StyleSheet" type="text/css" href="style.css">
</head>

<body>
<p class="mainhead"><a name="Top">Compiling the Demo Program</a></p>
  
<p class="heading"><a name="Compiling"><nobr>Compiling The Demo Program</nobr></a></p> 
 
<p>To compile your own version of the FrameBrowserIcs program for study or modification, the following is required:
  
<ol>
    <li>The source code for the demo program which is included in this package.<p>
   
    <li>If you have not already done so, download and install the <b>THtmlViewer</b>, <b>TFrameViewer</b>, 
        and <b>TFrameBrowser</b> HTML Components.  This package may be obtained from 
        <a href="https://github.com/BerndGabriel/HtmlViewer/" target="_blank"><code>github.com/BerndGabriel/HtmlViewer</code></a>.
  
    <li>This demo ICS v7 and the SSL files may be downloaded from
        <a href="http://wiki.overbyte.be/wiki/index.php/ICS_Download">http://wiki.overbyte.be/wiki/index.php/ICS_Download</a>   
</ol>

<!--  
<p class="heading"><a name="Compiling"><nobr>Compile Time Options</nobr></a></p> 

  <p>Depending on which options are selected, the following <B>Defines</B> should be entered
     in the Project|Options dialog, Directories/Conditionals tab, Conditional Defines 
     section:
  </p>
  <table width="80%" cellspacing="2" cellpadding="2" border="0">
  <tr> 
      <th width="2%"></th>
      <th class="dorange" valign="top" align="left" width="100">&nbsp;</th>
      <td valign="top" align="left">&nbsp;</td>
  </tr>
  </table>
-->

<p class="heading"><a name="Compiling"><nobr>Debugging</nobr></a></p> 

<p>Both ICS and TFrameBrowser use exception handling as part of normal coding practice
   so exceptions don't necessarily indicate a problem.

  <p>&nbsp;</p> 
</body>
</html>
�  0   H T M L   P A G E 4         0         <html>
<head>
	<title>Using the TFrameBrowser Component</title>
    <link rel="StyleSheet" type="text/css" href="style.css">
</head>

<body>
<p class="mainhead"><a name="Top">Using the TFrameBrowser Component</a></p>
  
<p class="heading"><a name="Using"><nobr>Overall Approach</nobr></a>

  <p>To use the TFrameBrowser component, you must have event handlers that respond to the <b>OnGetPostRequest</b> 
  and the <b>OnGetImageRequest</b> events.  These events occur whenever a document or an image for a document
  is requested.  These events require that the document or image be returned in stream form.  In some 
  cases, the return of the stream may be delayed to allow for downloading the information.
  </p>
  
  <p>The general sequence of loading a document is as follows:
  </p>
 
  <ol>
    <li>Call the LoadURL method.  The URL parameter supplied should be a full URL and include the 
        protocol.
    <li>An immediate <b>OnGetPostRequest</b> event will occur.  &nbsp;Respond with the stream requested.
    <li>Assuming the document contains images, a number of <b>OnGetImageRequest</b> events will occur.
        Again respond with a stream for each.
    <li>Once the document is loaded, additional events will occur when links are selected, etc.	  
  </ol>
  
  <p>Further information on the <b>OnGetPostRequest</b> and <b>OnGetImageRequest</b> events is available in the
  <code>THtmlViewer.chm</code> file.</p>
  
<p class="heading">Connections</p>
   
  <p>To obtain the document and image streams, the demo programs establish a <i><b>connection</b></i>. 
  A connection is obtained by calling 
  <code>TURLConnection.GetConnection</code> and the proper type of connection is determined by the protocol on the 
  URL used in the call.  It is through the connection concept that the various protocols are 
  supported.  The Connection concept makes it easy to 
  <a href="page5.htm#AddingProtocols" target="RightWin">add new protocols</a> for specialized situations.</p> 
  
  <p>Once the Connection is established, the stream may be obtained, usually with the 
  <code><b>Connection.Get</b></code> method.</P>

 
  <p>&nbsp;</p> 
</body>
</html>
�  0   H T M L   P A G E 5         0         <html>
<head>
    <title>Customizing - Adding Your Own Protocols</title>
    <link rel="StyleSheet" type="text/css" href="style.css">
</head>

<body>

<p class="mainhead"><a name="Top">Customizing</a></p>
  
<p class="heading"><a name="AddingProtocols"><nobr>Adding Your Own Protocols</nobr></a></p> 

  <p>The Connection concept makes it easy to add or modify protocols.  A few possible
  reasons that you might want to do this are:</p>
  
  <ul>
     <li>To read HTML from encrypted files
     <li>To read HTML from a database.
     <li>To use a different <b>http</b> component.</li>     
  </ul>
  
  <p>The various connection types are defined in <code>URLConId.pas</code>.  To add a new protocol,
  it's necessary to define a descendent to <code>TURLConnection</code>, the base class.
  In most cases, this is a two step process:</p>
  <ol>
     <li>Add your new protocol string to <code>TURLConnection.GetConnection</code>.  This makes
        your new connection available for use.
  
     <li>Override the <code>TURLConnection.Get</code> method. 
                The code you add here should use the URL to determine what 
                stream to return. It's also necessary to specify the content 
                type (HTMLtype, TEXTtype, or ImgType) of the return.</li>           
                     
                 
  </ol>
  <br>
  
  <p style="color: #9d3700;"><b>URL Format Considerations</b></font></p> 
  
  <p><b>TFrameBrowser</b> has the capability of assembling full URLs from partial URLs.  The
  syntax for doing this is detemined by the standard <b>http</b> and <b>file</b> protocols.  This
  means that any special protocols that are added should use URLs that resemble those
  used by the <b>http</b> and <b>file</b> protocols.</p><B></B>
  
  <p>The general form of a special URL should be:</p>
  
<pre>     proto://dir1/dir2/file.ext
  or
     proto:///file.ext</pre>  
     
  <p>The important points are:</p>
  <ul>
     <li>The protocol string should be followed by a colon and two (possibly three)
     forward slashes.
  
     <li>dir1/dir2/ represent a directory or other hierarchy.  The user can determine
     how this should be interpreted in his <code>Get</code> method code.
  
     <li>Whatever follows the last forward slash is taken as a document or image name
  
     <li>The URL should contain at least three 
                forward slashes. If there are only two, one will be added on the 
                end.</li>              
           
  </ul>
  <br>


<p class="heading"><a name="Resources"><nobr>Adding HTML documents to an Application's Resources</nobr></a></p>  

  <p>To construct a resource file containing HTML documents and images suitable for the <b>res</b> protocol, proceed as 
  follows:</p>
  <ul>
  	<li>Write a resource compiler file (<b>.rc</b>) listing the files to be included.
  	<li>Use the Borland Resource Compiler to compile the <b>.rc</b> file into a <b>.res</b> file.  The 
       Borland Resource Compiler is
  	    named BRCC32 (32 bit) or BRCC (16 bit).
  	<li>Reference the <b>.res</b> file in your program 
                code using the {$R } compiler directive.</li>             
  </ul>
  
  <p>A typical resource compiler file might look like:</p>
  
  <pre>  //   usage:  BRCC32.EXE -foHelp32.res proghelp.rc
  
  		agif2       GIF   &quot;agif2.gif&quot;
  		readme3     HTML  &quot;readme3.htm&quot;
  		readme1     HTML  &quot;readme1.htm&quot;
  		readme2     HTML  &quot;readme2.htm&quot;
  		leftwin     HTML  &quot;leftwin.htm&quot;
  		index       HTML  &quot;help.htm&quot;
  		style       HTML  &quot;style.css&quot;
  		borders	    TEXT  &quot;borders.txt&quot;
  </pre>
  
  <p>The first column is the name by which the resource is referenced.  Column two is the resource type.  The <b>res</b> 
  protocol as defined recognizes HTML, HTM, GIF, JPG, JPEG, BMP, PNG, and TEXT types.  The third column is the file
  which defined the HTML document.</p>
  
  <p>In the above, the URL, <b><code>res:///index.htm</code></b>, would load the resource defined by the file, 
  <b><code>help.htm</code></b>.
  </p></FONT>

  
<p>&nbsp;</p> 
</body>
</html>
   �  0   H T M L   P A G E 7         0         <html>
<head>
	<title>Acknowledgements</title>
    <link rel="StyleSheet" type="text/css" href="style.css">
</head>

<body>

<p class="mainhead"><a name="Top">Acknowledgements</a></p>
  <p>&nbsp;</p>

  <p>The following are some of the organizations and individuals who have contributed to the
  HTML components:<br>&nbsp;</p>
  
  <table align="center" cellspacing="2" cellpadding="2" border="0" width="75%">
<tr>
    <td height="30"><b>SkyLine Tools, Inc.</b></td>
    <td>ImageLib Graphics Library</td>
</tr>
<tr>
    <td height="30"><b>Radek Przybyl</b></td>
    <td>Ideas and code for establishing Print margins</td>
</tr>
<tr>
    <td height="30"><b>Theodor Kleynhans</b></td>
    <td>Animated GIF code</td>
</tr>
<tr>
    <td height="30"><b>Chris Wallace</b></td>
    <td>Print preview ideas and code</td>
</tr>
<tr>
    <td height="30"><b>Yves Urbain</b></td>
    <td>The Connection concept for protocol support</td>
</tr>
<tr>
    <td height="30"><b>Anders Melander</b></td>
    <td>Color dithering code in DitherUnit.Pas</td>
</tr>
<tr>
    <td height="30"><b>Gustavo Daud</b></td>
    <td>PngImage.pas and PngZLIB.pas</td>
</tr>
<tr>
    <td><b>Ron Collins</b></td>
    <td>GIF image code</td>
</tr>
<tr>
    <td><b>Mike Lischke</b></td>
    <td>Unicode routines and assistance</td>
</tr>
<tr>
    <td><b>jrsoftware.org</b></td>
    <td>Inno Installer</td>
</tr>
</table>

<p>&nbsp;</p> 
</body>
</html>
 >  0   H T M L   S T Y L E         0         

  body    {background:  white; font: 10pt Arial;}
  td, th  {font: 10pt Arial;}
  .backcolor  {background-color: #99ccff;}
  :link   {color: blue;} /*{color: #3737eb;}*/
  :hover  {color: blue;}
  :visited {color: blue;}
  code    {font-size: 1em;}
  .heading  {font: bold 12pt "Arial" ;  color: #ff6600;
             border-bottom: solid 1px #0000c0; width: 10; }
  .Mainhead {background-color: #ffd780;/*rgb(240, 192, 48);*/ color: blue;
             font: bold 20pt "Arial"; text-align: center; padding: 10px; Margin: 20;}
  .dorange  {color: #9d3700;}

    0   H T M L   X L E F T 1       0         <html>
<head>
	<title>Contents</title>
    <link rel="StyleSheet" type="text/css" href="style.css">
    <style>
       :link :visited {color: 3737a5;}
       .pindent  {margin-left: 10px;}
    </style>
</head>

<body>
<table border=0 width="150" cellpadding="0" cellspacing="0">
<tr><td valign="bottom" height="13"><img src="..\TabTop.gif" width="100%"></td>
<tr class="backcolor"><td>
<p style="text-align: center; font-size: 14pt; color: white; margin-top: 15px;">
<b>Contents</b>
<hr style="height: 6; margin: 10px; margin-top: 30px; color: 373773;">
<b>
<p class="pindent"><a target="RightWin" href="page1.htm">Overview</a>
<p class="pindent"><a target="RightWin" href="page3.htm">Compiling the Demo</a>
<p class="pindent"><a target="RightWin" href="..\page4.htm">Using The TFrameBrowser Component</a>
<p class="pindent"><a target="RightWin" href="..\page5.htm">Customizing</a>
<p class="pindent"><a target="RightWin" href="..\page7.htm">Acknowledge&shy;ments</a>
<p>
</b>
<tr><td valign="top" height="13"><img src="..\TabBot.gif" width="100%"></td>

<tr><td Height="50" valign="bottom"><p align="center"><a href="http://wiki.overbyte.be/wiki/index.php/ICS_Download" target="_top"><img src="ics_logo.png" align=center></a></td>

</table>

</body>

</html>
  @  4   P N G   I C S _ L O G O         0         �PNG

   IHDR   �   i  ��ʷ   `PLTE   x)r%&0(*y25:66�=?BLLQEE�PU�[\Zhhdfj�rqpx|���{�����������y���Ǿ�������������������|   	pHYs    ��~�   	tEXtComment  �*�  qIDATx��[�z�(�T"k������5�k���w/���e�*5�T7���Hi���~Ū^�Nu��u��Z���Y�?�����ìZ�Zy\��2R������C�+�[�:������������z���nC
��v�vg����32V7M#�]�]]b�KF�l�5aq>�%?M�A�m�8~<��B���۝d14I���=9i!�uP.<�E~�KA��I�kGmB$Ե�@�t���Ǭ���j�h�H}�?q)�PB��vZ/s�d�JSL����U���( �G��	�%����!0Pd�l�I�|i��q�s' ��HY�G��.�dt)?�̌k�d��U�O���O����f\g%VU-WZ�����h�Z����'�m���ys�̲Ũ�]�{�^A�J��s{̢�+R,�1��iA-��<���\��R�X۴,2�!�(�s�uq���֘V���|�XB]E�#AX蚇�[�1%��1�/k��г��� $(�?�Q�	�R��_��k"� �	���s�5���f^� Mx0��OϪ��{�5&[���8�_=�����T���[K��Δ�W�_|��O6�P��;�6�W�i�J�<�ZG���6��p>��ۍ�]�+��te���cێ�9�)��"����,��I>nua},��<��κ\vk9���`���1�)C.w^g�&�u}B&���=g�$��<�ee(tϽ�@��V��iUg��u	2��2�eŝ�sk`�s��~zX!��	02m��C7���8fy��h�ɉm�m?��Y��Vd����{���OZ!��=��4�CFG�OK���GG��1yca��\��^�:<*���MB/$��M�&B<��IaVz���� �u�UD�Z�Z���,|9�תa�1��Y����Y�z��X!B���_g�0�a���u�&<��g��:+Q.�_�_̫g�^�l���s׈l9���}]��V#����d��E��*N[�r��} B��X|!R�(�|���(6xK>m#��.~� )��n�k_g�,H��O㫨��\}8�~m[�֗l�k���M�L�ML��sx#*z$���F>-|Q�qV���%=Է!��uI�O�%Uu[��ڶOВBZ��ʿ�%Cd��L�'Q)�q�&�
�0��I�s��t�*޷�Q��,*zn�Q�|���v���.�ZMHϢ`q͸�gs�������(آU���g�����?�
�6+�88�]W�2��؍v���IJ�(m�`�C�㳨��!,�
^���*\�۠�%��+� ���+*�0$"��SZ��%�͂>"jG��9���qX�6K�������ڙn�C��E�#P�K��2���������\^A�5��j�C�&�E�e}X]7`TvUD-�.�;PQ*LɯZ+$E���s�6�Щ�����D�����߬>�/Mt��{�'��������=(Ĵ����x�����%'���G���]��]��\�#�%��ۛ�n�2R4�-7����[����5�$2;f��#:�4���Յ�D��:�W���ʐ��ET�B�����F[\C�LxM��dG���~1#8s���Q�����m���(���f�ET��I;3i�NZ9�Si�'[��;#���=��7���β�/�(R��0���P�G82�e�thz@zP+�ϡ�(��sJ9"�R%M�(���4o��2����l]�j&�y):���d�`	F����xؒ�|���Ln�ˊ:K�i��/��O�6���&��蓷y���	#����	i�	'�a�D<��6��b 2~P��R��ʜ�:�c����F���NTJG���_�������>ZA��E�<U\�U?��I����G2��@�������L�B�ǩm9_���z[d��Q�{��Eђ�^$�Ϫ��>� �P�n��dY1��{n�����~���ٝ�i�vۘqQӜ�זǪ�o�W&]I�g��a\�o��xU����}
sh\z�Jj�_J�,���� �����ǋؒ�쳨����z�~�%m��6�k|eDM捨�B�l@��
���A����ռC�¢
�[���ָ����_D�5�Z��1Ы馫�f��KΛ���B�i3T���O��Q
a�,!ChB��
�EL��(�@e���
@KE��Ԓ/�Q��nDu�^�����"5�����<1�*�|��.'r��#�ժ	���`�	��^��!K_QAMQ��Ţ1�4��R�sP�!�߈������Ȉ�
	FK���ͦϨχ�}��J�?�Ij)Y�FZB&��`P󡠣�\�LEPͬz]���B�T1	�Յ2z.�K+e"�i�.m���Y�45|����dE�Ï�O����2ӹE�ʈMx�}�d�:���D
X����V��T:6L�=h*{�\����EE�Vb��]�,Ih��+��
��Kd�V�@��{�|�bbv�ۢ��AQ�k�j
).�V��Ji�G��>I
�u:}<Fђ#�XrQ{�`�Q-^!؁��'����P����}*x�菵�c4)?�r:���6S�q�y1�����e͵La9�*xT>�S�]}[���GދV=m�?oy0�E��[��K���ۛ,
*�	Ia��$4+*�B`����EJ�H�-���d��b�E�	:�ƾGu�T�,�[W��3|� n%@*�x�l�����p����d;�x��f;�n�GwA�r��Q�?���I T������F�3�!�-��`�^�qu��s�afN��(V�Q�Z�s��z��[S�(��Y��*	�Q/$
��QkT"T�u�!�Q�aH+���Ԏ�*�=*J��t�Խ
�)���RG� ��P�-$��꽨��^k� �J+�s�������*���Di�����qm4�}#<�T�"#Sj4���*j�qlh@�dc�Q�@�Fj��S)Gn��������**4�5��Z4H�5kf��I��\x��Z�_Yn\^EE����F��ɯ]�s���_
V�BDd�Z-�9�__��מ������v 4���~,�Cy��)�<D]v�I?a���+gw
�?��A[!��Ό��hQ�~L�,O�P�%ɨ;l�B1��M����ZK�v��2wQ�s���`WQ�ZZ52+*K�!���B���m��)wms疊L{d��^��gbY��
ͩY���X�-�A���>���>���W��!*����J@iE�S옧>��PN5�/��A�fittߕ��Ò��J�%�\7K�4�-���lvN����(�t���5�C�ԡq׮��G������vN��O=i��Byuq-�K(��2���o�2�[`��{@!��d �(�!������{�b���&�����v���a��~����A̯�2"�NU5ɮo��T}pT�
�zi���y+$�l4�e��������������F����8�|V�9��D������$��^ѽ������9׿b���T�4���17ޠ&T�����p��X�W=i�P�/��~��##8�y�����^-��ڕ#�/󺜣ق;��w%�O��í�q�!l�����	��X�z>>O�<�m;R���i���sn�mn�Rb�h�=�Z�EK3�w��{i�L��Կ�0Mm�J�i�&u��	�mC>�|� u�շ�P����Iߪ�_@���XƲ,���@e�W��o��KK����dT?���A���MSd�bR��#O�'�Y�s�v;�-��21].��l�`��<V���YS�Q���p�q�����L?�35��q��&�&w`��v�~ �d�G��R�A�a���_��?��:��Q9@��`E��i�g��6m�Bg63�e�j��·���.!og?�
�(;�����>J=��X�#Nt����י��E�B�Ȫ~ �HP*�P����5E�,��ժ$�׾WnA9G�gV9�)e�=�������iPkl����@�|��u-
��Yq7�Z%��2j�����[�4/�a�ض�:���L���gz�Ք)Ջ-ASy���9��� v�����kHkw��|��yʛ��_��)�X����B�/�gPdߴ���)Q �&M�����U*��s�ԙ�����B�~5���g��x�U1G@��sK%��<RRv�Ճ0u�J�w�#m�o���W^��:6Wկ!���羆=��FӶ@�.�^�E�&ڿs�^]ޞ%P��;��>5GL�(�l	ΰ�a��������'��(#�]7,-�Qvᓦ�@��]qS4������;�l@�a�`N����U��̤�]���Pp�3*G�Hyv0tP��c�O�ZK�
HB����5��0gǛY[��'�oA�w�"ϧW(�#�[�8�_�l�^��Ǯ�G?"���Ƴ���W�򯚊Ӻ��ps=�u��<xARʅk�L�:o�V�ߐ�������֜�)i��N��ME:������h�ŻV|�5%����M���뺡u�����}��VW��lz�qհn>ߢZ	����4���u��R{Zx�g.C�NM7��x)�Aþ��n�|�w��h�С	M[F��
���.(�F� �痠z���X6����*�w%V4��5yt��覦��.Qӎ���l�1|�@�,��"�Qt�KkL��.t�,��ԛ�� �����>O�0_�Q����*]U��L�L�YW�x������P�����tM�R�Fպ��8�U�=�iQ�F�7�_�c��v�z���'��7�7'P�*@��!��:å@/*L��!� ��;�����ڣ�K���1^i:��?�'�(�$/�r2���2��埨a��{8$?�~=M�`����_�)����pgW�F7�f����0;V�l� �}ckS�l�@!�S�N��ڏ�4�������P����wP���f�l��E�d��f=0�
$]0cM�L�0�\����R<��Y>�=��՚�TH�@]�P���H�����V�$4�$� E/A*�1�@��./_�z�o�M�
�ɩ�@��e(����ί�c�G{�?��L��M%��-Ԡ��N�E�.�:����9Z=.	����MtH쯠�U����س��B�PQ�*ɵ�??ݷm�p�͢2(�pRӹ�����	�Գ��H�1,��,� ����y0�����|�-P�HQ)��\���ar��
��)1����A�AS�l``J*g+�y��Ѐ��P�X��}�T���8��o�$���$� �0S@���o��,�l�Ѱn�W:h	�Q3��<��5�0���Ds��c����:���B��ԶӪ��d���WP���`b��Ǡ���8z[���t��ɱ������Q��J��������M�[�����3?]�yf	 ����
�p�"���B�	Ԙ+�|�����������a���¼��U,ʙ�G�z�E�\-[���P����H�Y�*#���ac�ynp\viኧ��yS����z���������1i���Yog�t�d��L<�l*h��*�1���y�Aۣ��]p�vy�Q���� �{�N8��N���0�_�fAa�h_��>_,��V��#.���._@E_���(�-�s�мj*���+� %���aL�ms��,��ҭ������/e��x��օ-t��(�<?�ǂ�i����<^���p���x3�GPD�3�����GY�o�#^�X��P+�M�Sp��n(���R�){���&*�m�l�=���� O��Gb�I�5/8�(��,�� 5jg��$��y��l:�e�H��_4H|��<?�)�_Fd �t��C��)�f�9�����&�LÅE��12ODrF�ct3k'f!_�ￍ�2m�g
���L�}�x>\͕�K�y�/T��H�� j�����_�H<Nl_�eƛ���� � ��F��!    IEND�B`��   0   G I F   T A B B O T         0         GIF89a�  �  ������������������������                        ,    �   S��I��8�ͻ�`(�di�h��l�h�t[u����� ���!�����As�@�u{2�`@���9�7�n���|N��# ;  �   0   G I F   T A B T O P         0         GIF89a�  �  ������������������������                        ,    �   T�I��8�ͻ�`@Gi�h��l�p�D����|��e�+�HՠBH:��a"�Z�X���&U�x�����n���|N��K ; <  4   A V I   S T A R C R O S S       0         RIFF4  AVI LIST�  hdrlavih8   @  X                 x        �?^ �?  �l  ��lLISTt  strlstrh8   vidsDIB             @ @B        h  ����            strf(  (               h                     3   f   �   �   �    3  33  f3  �3  �3  �3   f  3f  ff  �f  �f  �f   �  3�  f�  ��  ̙  ��   �  3�  f�  ��  ��  ��   �  3�  f�  ��  ��  ��    3 3 3 f 3 � 3 � 3 � 3  33 333 f33 �33 �33 �33  f3 3f3 ff3 �f3 �f3 �f3  �3 3�3 f�3 ��3 ̙3 ��3  �3 3�3 f�3 ��3 ��3 ��3  �3 3�3 f�3 ��3 ��3 ��3   f 3 f f f � f � f � f  3f 33f f3f �3f �3f �3f  ff 3ff fff �ff �ff �ff  �f 3�f f�f ��f ̙f ��f  �f 3�f f�f ��f ��f ��f  �f 3�f f�f ��f ��f ��f   � 3 � f � � � � � � �  3� 33� f3� �3� �3� �3�  f� 3f� ff� �f� �f� �f�  �� 3�� f�� ��� ̙� ���  ̙ 3̙ f̙ �̙ �̙ �̙  �� 3�� f�� ��� ��� ���   � 3 � f � � � � � � �  3� 33� f3� �3� �3� �3�  f� 3f� ff� �f� �f� �f�  �� 3�� f�� ��� ̙� ���  �� 3�� f�� ��� ��� ���  �� 3�� f�� ��� ��� ���   � 3 � f � � � � � � �  3� 33� f3� �3� �3� �3�  f� 3f� ff� �f� �f� �f�  �� 3�� f�� ��� ̙� ���  �� 3�� f�� ��� ��� ���  �� 3�� f�� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� ��� vedt         JUNK�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              LIST  moviLIST�  rec 00dbh  ��������������  ����������  ��������  ��������  ������  ����������  ��  ��  ������������������  ������������������  ��  ��  ����������  ������  ��������  ��������  ����������  ��������������  JUNK|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              LIST�  rec 00dbh  �������������  ��������  �����������  ������������  �������  �������  ��������  ����������  �������  ��������  ��������  �����  �������  ���������  �������������  �����������  ���������  ��������������  JUNK|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              idx1@   rec       �  00db      h  rec      �  00db     h  