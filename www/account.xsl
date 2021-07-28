<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/account'>
	<html>
		<head>
			<title>Account</title>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<form method='POST' action='account'>
				<label>
					New password
					<input type='password' name='password'/>
				</label>
				<button type='submit'>Change</button>
			</form>
			<p>Current avatar</p>
			<img>
				<xsl:attribute name='src'>/avatar/<xsl:value-of select='session/name'/></xsl:attribute>
			</img>
			<form method='POST' action='avatar' enctype="multipart/form-data">
				<xsl:attribute name='action'>/avatar/<xsl:value-of select='session/name'/></xsl:attribute>
				<label>
					Change Avatar
					<input type='file' name='avatar'/>
				</label>
				<p><i><small>Avatar image should be square, and will be scaled down to 64px x 64px</small></i></p>
				<input type="submit" />
			</form>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
