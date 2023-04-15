<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <!--
       Copyright (C) 2023 Simon Wright <simon@pushface.org>
       SPDX-License-Identifier: GPL-3.0-or-later
       -->

  <xsl:strip-space elements="*"/>
  <xsl:output method="text"/>

  <xsl:template match="PackageDecl">
    <xsl:variable name="package-name" select="DefiningName/Id"/>
    with Scripted_Testing.Stubs;
    package body <xsl:value-of select="$package-name"/> is
    <xsl:apply-templates
      select="PublicPart//SubpSpec"
      mode="stub">
      <xsl:with-param name="package-name" select="$package-name"/>
    </xsl:apply-templates>
    begin
    <xsl:apply-templates
      select="PublicPart//SubpSpec"
      mode="register">
      <xsl:with-param name="package-name" select="$package-name"/>
    </xsl:apply-templates>
    end <xsl:value-of select="$package-name"/>;
    <xsl:apply-templates
      select="PublicPart//SubpSpec"
      mode="body">
      <xsl:with-param name="package-name" select="$package-name"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="SubpSpec[SubpKindFunction]" mode="stub">
    function <xsl:value-of select="DefiningName/Id"/>
    <xsl:apply-templates select="Params" mode="ada"/>
    return <xsl:value-of select="SubtypeIndication/Id"/> is separate;
  </xsl:template>

  <xsl:template match="SubpSpec[SubpKindFunction]" mode="register">
    <xsl:param name="package-name"/>
    <xsl:variable name="n" select="DefiningName/Id"/>
    <xsl:variable name="subp-name" select="concat($package-name, '.', $n)"/>
    Scripted_Testing.Stubs.Register_Subprogram
    (&quot;<xsl:value-of select="$subp-name"/>&quot;);
    <xsl:apply-templates
      name="Params"
      mode="register">
      <xsl:with-param name="subp-name" select="$subp-name"/>
    </xsl:apply-templates>
    Scripted_Testing.Stubs.Register_Output_Parameter
    (&quot;<xsl:value-of select="$subp-name"/>&quot;, &quot;return&quot;);
  </xsl:template>

  <xsl:template match="SubpSpec[SubpKindFunction]" mode="body">
    <xsl:param name="package-name"/>
    <xsl:variable name="name" select="DefiningName/Id"/>
    <xsl:variable name="subp-name" select="concat($package-name, '.', $name)"/>
    separate (<xsl:value-of select="$package-name"/>)
    function <xsl:value-of select="$name"/>
      <xsl:apply-templates select="Params" mode="ada"/>
      return <xsl:value-of select="SubtypeIndication/Id"/> is
      Lock : Scripted_Testing.Stubs.Lock (Scripted_Testing.Stubs.Mutex'Access);
      pragma Unreferenced (Lock);
      Call : constant Positive := Scripted_Testing.Stubs.Note_Entry
      (&quot;<xsl:value-of select="$subp-name"/>&quot;);
    begin
      <xsl:call-template name="body-content">
        <xsl:with-param name="full-name" select="$subp-name"/>
      </xsl:call-template>
      <!-- Extract the return value -->
      return <xsl:value-of select="SubtypeIndication/Id"/>'Input
        (Scripted_Testing.Stubs.Get_Output_Value_Stream
        (&quot;<xsl:value-of select="$subp-name"/>&quot;, &quot;return&quot;, Call));
    end <xsl:value-of select="$name"/>;
  </xsl:template>

  <xsl:template match="SubpSpec[SubpKindProcedure]" mode="stub">
    procedure <xsl:value-of select="DefiningName/Id"/>
      <xsl:apply-templates select="Params" mode="ada"/> is separate;
  </xsl:template>

  <xsl:template match="SubpSpec[SubpKindProcedure]" mode="register">
    <xsl:param name="package-name"/>
    <xsl:variable name="n" select="DefiningName/Id"/>
    <xsl:variable name="subp-name" select="concat($package-name, '.', $n)"/>
    Scripted_Testing.Stubs.Register_Subprogram
    (&quot;<xsl:value-of select="$subp-name"/>&quot;);
    <xsl:apply-templates
      name="Params"
      mode="register">
      <xsl:with-param name="subp-name" select="$subp-name"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="SubpSpec[SubpKindProcedure]" mode="body">
    <xsl:param name="package-name"/>
    <xsl:variable name="name" select="DefiningName/Id"/>
    <xsl:variable name="subp-name" select="concat($package-name, '.', $name)"/>
    separate (<xsl:value-of select="$package-name"/>)
    procedure <xsl:value-of select="DefiningName/Id"/>
    <xsl:apply-templates select="Params" mode="ada"/> is
      Lock : Scripted_Testing.Stubs.Lock (Scripted_Testing.Stubs.Mutex'Access);
      pragma Unreferenced (Lock);
      Call : constant Positive := Scripted_Testing.Stubs.Note_Entry
      (&quot;<xsl:value-of select="$subp-name"/>&quot;);
    begin
    <xsl:call-template name="body-content">
      <xsl:with-param name="full-name" select="$subp-name"/>
    </xsl:call-template>
    end <xsl:value-of select="$name"/>;
  </xsl:template>

  <xsl:template match="Params" mode="ada">
    (<xsl:for-each select=".//ParamSpec">
     <xsl:value-of select=".//DefiningName/Id"></xsl:value-of> :
     <xsl:choose>
       <xsl:when test="ModeIn"> in </xsl:when>
       <xsl:when test="ModeOut"> out </xsl:when>
       <xsl:when test="ModeInOut"> in out </xsl:when>
     </xsl:choose>
     <xsl:value-of select="SubtypeIndication/Id"/>
     <xsl:if test="not (position() = last())">;</xsl:if>
    </xsl:for-each>)
  </xsl:template>

  <xsl:template match="Params" mode="register">
    <xsl:param name="subp-name"/>
    <xsl:for-each select=".//ParamSpec[ModeDefault | ModeIn | ModeInOut]">
      Scripted_Testing.Stubs.Register_Input_Parameter
      (&quot;<xsl:value-of select="$subp-name"/>&quot;,
       &quot;<xsl:value-of select=".//DefiningName/Id"/>&quot;);
    </xsl:for-each>
    <xsl:for-each select=".//ParamSpec[ModeOut | ModeInOut]">
      Scripted_Testing.Stubs.Register_Output_Parameter
      (&quot;<xsl:value-of select="$subp-name"/>&quot;,
       &quot;<xsl:value-of select=".//DefiningName/Id"/>&quot;);
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="body-content">
    <!--  Called at SubpSpec to save the input parameters, check for
         exceptions, and extract the output parameters. -->
    <xsl:param name="full-name"/>
    <!-- Save all the input parameters -->
    <xsl:for-each select="Params//ParamSpec
                          [ModeDefault | ModeIn | ModeInOut]">
      <xsl:variable name="param" select=".//DefiningName/Id"/>
      <xsl:value-of select="SubtypeIndication/Id"/>'Output
      (Scripted_Testing.Stubs.Get_Input_Value_Stream
      (&quot;<xsl:value-of select="$full-name"/>&quot;, &quot;<xsl:value-of select="$param"/>&quot;, Call, <xsl:value-of select="$param"/>'Size),
      <xsl:value-of select="$param"/>);
    </xsl:for-each>
    <!-- Is an exception required? -->
    Scripted_Testing.Stubs.Check_For_Exception
    (&quot;<xsl:value-of select="$full-name"/>&quot;, Call);
    <!-- Extract all the output parameters -->
    <xsl:for-each select="Params//ParamSpec
                          [ModeOut | ModeInOut]">
      <xsl:variable name="param" select=".//DefiningName/Id"/>
      <xsl:value-of select="$param"/> :=
      <xsl:value-of select="SubtypeIndication/Id"/>'Input
      (Scripted_Testing.Stubs.Get_Output_Value_Stream
      (&quot;<xsl:value-of select="$full-name"/>&quot;, &quot;<xsl:value-of select="$param"/>&quot;, Call));
    </xsl:for-each>
  </xsl:template>

  <!-- Skip over leading pragmas, context. Why doesn't match="*" work? -->
  <xsl:template match="/CompilationUnit/AdaNodeList"/>

  <xsl:template match="*" mode="stub"/>
  <xsl:template match="*" mode="register"/>
  <xsl:template match="*" mode="body"/>
  <xsl:template match="*" mode="ada"/>

</xsl:stylesheet>
