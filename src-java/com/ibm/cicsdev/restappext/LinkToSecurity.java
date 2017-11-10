/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2017 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */

package com.ibm.cicsdev.restappext;

import java.security.AccessController;
import java.security.Principal;
import java.util.Iterator;
import java.util.Set;

import javax.security.auth.Subject;
import javax.security.auth.login.CredentialExpiredException;

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;
import com.ibm.websphere.security.WSSecurityException;
import com.ibm.websphere.security.auth.CredentialDestroyedException;
import com.ibm.websphere.security.auth.WSSubject;
import com.ibm.websphere.security.cred.WSCredential;
import com.ibm.wsspi.security.credentials.saf.SAFCredential;

/**
 * Provides an example of deploying a POJO into Liberty to allow the
 * use of CICS LINK into a Java EE environment.
 *
 * Link to Liberty is not supported in environments earlier than
 * CICS V5.3 with APAR PI63005.
 *
 * To successfully compile this source file, you will need to upgrade your
 * CICS Explorer or CICS Build Toolkit environment to V5.3.0.8 or later.
 */
public class LinkToSecurity
{
    
    @CICSProgram("L2LSEC")
    public void getSecurityInfo() throws CicsConditionException, WSSecurityException, CredentialExpiredException, CredentialDestroyedException
    {

    	Subject activeSubject = Subject.getSubject(AccessController.getContext());
        String userName = "NOSUBJCT";  // Value to use if Subject is null.
        if (activeSubject != null) {
            Set<Principal>  principalSet = activeSubject.getPrincipals();
            
            // Get just the first Principal from the Set, if any
            Iterator<Principal> principalIterator = principalSet.iterator();
            Principal principal = principalIterator.next();
            
            if (principal != null) {  // Get name from Principal.
            	userName = principal.getName();
            }
            else userName = "NOTKNOWN"; // No Principal in Subject.
        }
        
        // Obtain the Userid from the CICS Task
        String taskuserid = Task.getTask().getUSERID();

        // Append taskuserid & userName to StringBuffer, padding each to 8 chars:
        StringBuffer sb = new StringBuffer();
        sb.append(taskuserid);
        for (int i=taskuserid.length() ; i < 8 ; i++) sb.append(' ');
        sb.append(userName);
        for (int i=userName.length() ; i < 8 ; i++)   sb.append(' ');
       
        // Return sb content in CHAR container CONT-IDENTITY in current channel.
        Channel ch = Task.getTask().getCurrentChannel();
        Container contIdentityRecord = ch.createContainer("CONT-IDENTITY");
        contIdentityRecord.putString(sb.toString());

        //**********************************************************************
        // An alternative method to obtain the user information from the Subject:
        String publicUserid="",realm="",privateUserid="",userinfo="";
        WSCredential wsCred;
        Subject callerSubject = WSSubject.getCallerSubject();
		if (callerSubject != null) {

			// get the public credential 					
			Set<WSCredential> wsCredentials = callerSubject.getPublicCredentials(WSCredential.class);
			Iterator<WSCredential> wsCredentialsIterator = wsCredentials.iterator();
			if (wsCredentialsIterator.hasNext()) {
				wsCred = wsCredentialsIterator.next();
				publicUserid = wsCred.getSecurityName();
				realm = wsCred.getRealmName();
			}									
			// publicUserid = callerSubject.getPrivateCredentials(WSCredential.class).iterator().next().getSecurityName();

			    // Get the private credential from the subject
			    privateUserid = callerSubject.getPrivateCredentials(SAFCredential.class).iterator().next().getUserId();

			userinfo = "Realm:" + realm + " userid:" + publicUserid;				

			if (!privateUserid.equalsIgnoreCase(publicUserid)) {
				userinfo = userinfo + " private userid:" + privateUserid;
			} 										
		}	 
		// Output user info to JVM sysout log.
		System.out.println(userinfo);
    }
}
