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

import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.Principal;
import java.util.Iterator;
import java.util.Set;

import javax.security.auth.Subject;

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;

/**
 * Class containing Link to Liberty method(s).
 * See LinkToLiberty.java for more information.
 */
public class LinkToSecurity
{
    /**
     * Link to Liberty enabled method to demonstrate security context
     * propagation to the Java thread and to the CICS Liberty task.
     * 
     * Called as program L2LSEC by COBOL program LINK2SEC.cbl
     * 
     * @throws CicsConditionException
     */
    @CICSProgram("L2LSEC")
    public void getSecurityInfo() throws CicsConditionException
    {
        // Get the current access control context.
        AccessControlContext context = AccessController.getContext();
    
        // Get the Subject from context.
        Subject activeSubject = Subject.getSubject(context);
        
        // The name of the Principal, if any.
        String principalName;  
        
        if (activeSubject != null) {
        
            // The Subject exists; get the Set of Principals.
            Set<Principal>  principalSet = activeSubject.getPrincipals();
            
            // Get just the first Principal from the Set, if any.
            Iterator<Principal> principalIterator = principalSet.iterator();
            Principal principal = principalIterator.next();
            
            if (principal != null) {
                // Get the name from Principal.
                principalName = principal.getName();
            }
            else {
                // No Principal in Subject.
                principalName = "UNKNOWN"; 
            }
        }
        else {
            // Value to use if Subject is null.
            principalName = "NOSUBJECT";  
        }
        
        // Get the current channel to add the response containers.
        Channel ch = Task.getTask().getCurrentChannel();
        
        // Create container for the Principal name.
        Container contPrincipal = ch.createContainer("PRINCIPAL");
        
        // Put the Principal name as a CHAR container.
        contPrincipal.putString(principalName);

        // Create container for the CICS task userid.
        Container contUserid = ch.createContainer("USERID");
        
        // Obtain the Userid from the CICS Task
        String cicsUserid = Task.getTask().getUSERID();

        // Put the userid as a CHAR container. 
        contUserid.putString(cicsUserid);
    }
}
