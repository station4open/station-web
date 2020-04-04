<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='/menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/subject'>
	<html>
		<head>
			<title>Station - Subject</title>
			<xsl:call-template name='header-head'/>
			<xsl:call-template name='slide-head'/>
			<link rel='stylesheet' type='text/css' href='../subject.css'/>
		</head>
		<body>
			<xsl:call-template name='header'/>
			<xsl:call-template name='slide'/>
			<div class='container-content'>
				<div class='container-subject'>
					<div class="title">
						<xsl:value-of select='title'/>
					</div>
					<div class='btn'>
						<a href='/home'>
							View All Course<xsl:if test='count(courses/course)!=1'>s</xsl:if>
						</a>
					</div>
				</div>
				<div class='container-description'>
					<span><xsl:value-of select='description'/></span>
				</div>
				<div id='all_course' class='title'>
					All Course<xsl:if test='count(courses/course)!=1'>s</xsl:if>
				</div>
				<div class='content'>
					<xsl:for-each select='courses/course'>
						<div class='card-course'>
							<div class='title'>
								<xsl:value-of select='title'/>
							</div>
							<div class='description'>
								<span><xsl:value-of select='description'/></span>
							</div>
							<div class='tail'>
								<div class='enroll'>
									<a>
										<xsl:attribute name='href'>../course/<xsl:value-of select='identifier'/></xsl:attribute>
										Enroll Now
									</a>
								</div>
								<div class='point'></div>
							</div>
						</div>
					</xsl:for-each>
				</div>
			</div>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
