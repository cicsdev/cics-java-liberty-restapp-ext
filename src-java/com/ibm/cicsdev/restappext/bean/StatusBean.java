/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2017 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */

package com.ibm.cicsdev.restappext.bean;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Simple data access bean, used to serialise the required data to JSON using
 * the JAX-RS interfaces.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "status")
public class StatusBean
{
    @XmlElement(name = "message")    
    private String message;
    
    /**
     * Constructor to initialise this instance using a single status message.
     * 
     * @param strMessage The message to encapsulate.
     */
    public StatusBean(String strMessage) {
        
        // Simple initialisation
        this.message = strMessage;
    }

}
