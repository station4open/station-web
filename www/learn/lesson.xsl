<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet
	version='1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns='http://www.w3.org/1999/xhtml'
>
<xsl:import href='../menubar.xsl'/>
<xsl:output method='xml' version='1.0' encoding='UTF-8' indent='yes'/>
<xsl:template match='/lesson'>
	<html>
		<head>
			<title>Station - Lesson</title>
			<xsl:call-template name='header-head'/>
			<link rel='stylesheet' href='../lesson.css'/>
			<script type='application/ecmascript' src='../swap.js'  async=''/>
			<script type='application/ecmascript' src='../lesson.js' async=''/>
		</head>
		<body>
			<xsl:call-template name='header'/>
			<div class='container-content'>
				<div class='container-title'>
					<div class='title-content'>
						<div class='btn-back'>
							<a>
								<xsl:attribute name='href'>../course/<xsl:value-of select='course'/></xsl:attribute>
								<span>X</span>
							</a>
						</div>
						<div class='lesson-title'>
							<span>
								<xsl:value-of select='number'/>. <xsl:value-of select='title'/>
							</span>
						</div>
					</div>
					<div class='container-swap'>
						<input type='checkbox' id='swap' name='swap'/>
						<label class='btn-swap' for='swap'/>
					</div>
				</div>
				<div class='content'>
					<div class='container-article content-content'>
						<div class='session-title'>
							<span>Article</span>
						</div>
						<div class='content-article content-container'>
							<div class='article'>
								<span>
									<xsl:value-of select='content'/>
								</span>
							</div>
						</div>
						<div class='content-embed content-content'>
							<xsl:for-each select='embeds/embed'>
								<div class='embed'>
									<xsl:choose>
										<xsl:when test='kind=1 or kind=2'>
											<img>
												<xsl:attribute name='src'>/learn/embed/<xsl:value-of select='identifier'/></xsl:attribute>
											</img>
										</xsl:when>
										<xsl:when test='kind=101'>
											<iframe
												width="560" height="315"
												title="YouTube video player"
												frameborder="0"
												allow="accelerometer; encrypted-media; gyroscope; picture-in-picture; fullscreen"
												allowfullscreen=''
											>
												<xsl:attribute name='src'>https://www.youtube.com/embed/<xsl:value-of select='value'/></xsl:attribute>
											</iframe>
										</xsl:when>
									</xsl:choose>
									<xsl:value-of select='title'/>
								</div>
							</xsl:for-each>
						</div>
					</div>
					<div class='container-question content-content'>
						<div class='session-title'>
							<span>Questions</span>
						</div>
						<form method='POST'>
							<xsl:attribute name='action'>
								<xsl:value-of select='identifier'/>
							</xsl:attribute>
							<xsl:for-each select='questions/question'>
								<div class='content-question content-container'>
									<div class='question-text'>
										<span>
											Q<xsl:value-of select='number'/>. <xsl:value-of select='text'/>
										</span>
										<div class='mark'>
											<span>
												Full mark:
												<xsl:value-of select='mark'/> point<xsl:if test='mark!=1'>s</xsl:if>
											</span>
										</div>
									</div>
									<div class='container-answer'>
										<xsl:for-each select='answers/answer'>
											<div class='answer'>
												<label class='answer-radio'>
													<input type='radio' required=''>
														<xsl:if test='not(../../../../session)'>
															<xsl:attribute name='hidden'/>
														</xsl:if>
														<xsl:attribute name='name'>
															<xsl:value-of select='../../identifier'/>
														</xsl:attribute>
														<xsl:attribute name='value'>
															<xsl:value-of select='identifier'/>
														</xsl:attribute>
														<xsl:if test='@answered'>
															<xsl:attribute name='checked'/>
														</xsl:if>
													</input>
													<xsl:value-of select='text'/>
													<span></span>
												</label>
												<xsl:if test='@answered'>
													<span>
														(<xsl:value-of select='@answered'/> point<xsl:if test='@answered!=1'>s</xsl:if>)
													</span>
												</xsl:if>
											</div>
										</xsl:for-each>
									</div>
								</div>
							</xsl:for-each>
							<xsl:if test='session'>
								<div class='container-submit'>
									<div class='btn-submit'>
										<input type='submit' value='Submit'/>
									</div>
								</div>
							</xsl:if>
						</form>
					</div>
				</div>
			</div>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
