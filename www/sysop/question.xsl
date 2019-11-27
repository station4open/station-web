<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/question'>
	<html>
		<head>
			<title>SysOp: Lesson</title>
			<link rel="stylesheet" type="text/css" href="../base.css"/>
			<script type='application/ecmascript' src='../../../delete_disable.js' async=''/>
		</head>
		<body>
			<xsl:call-template name='menubar'/>
			<a>
				<xsl:attribute name='href'>../lesson/<xsl:value-of select='lesson'/></xsl:attribute>
				Back
			</a>
			<section>
				<h1>Question</h1>
				<form method='POST'>
					<xsl:attribute name='action'>../question/<xsl:value-of select='identifier'/></xsl:attribute>
					<input type='hidden' name='lesson'>
						<xsl:attribute name='value'><xsl:value-of select='lesson'/></xsl:attribute>
					</input>
					<label>
						Question
						<div class='flex'>
							<textarea name='text'><xsl:value-of select='text'/></textarea>
						</div>
					</label>
					<label>Delete<input type='checkbox' name='delete'/></label>
					<br/>
					<button type='submit'>Modify</button>
				</form>
			</section>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
