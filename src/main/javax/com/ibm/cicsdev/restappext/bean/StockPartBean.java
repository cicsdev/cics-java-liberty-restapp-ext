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

import com.ibm.cicsdev.restappext.generated.StockPart;

/**
 * Simple data access bean, used to serialise the required data to JSON using
 * the JAX-RS interfaces.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "stockPart")
public class StockPartBean
{
    @XmlElement(name = "partId")    
    private int iPartId;
    
    @XmlElement(name = "supplierId")
    private int iSupplierId;

    @XmlElement(name = "stockQuantity")
    private int iStockQuantity;
    
    @XmlElement(name = "lastOrderDate")
    private String lastOrderDate;
    
    @XmlElement(name = "nextOrderDate")
    private String nextOrderDate;
    
    @XmlElement(name = "unitPrice")
    private String strPrice;
    
    @XmlElement(name = "description")
    private String strDescription;
    
    
    /**
     * Constructor to initialise this instance using values provided by
     * the supplied {@link StockPart} instance.
     * 
     * @param sp The object used to initialise this instance.
     */
    public StockPartBean(StockPart sp) {
    
        // Pull all of the values into the internal representation
        this.iPartId = sp.getPartId();
        this.iSupplierId = sp.getSupplier();        
        this.strDescription = sp.getDescription().trim();
        this.iStockQuantity = sp.getStockQuantity();
        this.strPrice = sp.getUnitPrice().toPlainString();        
        
        // Last order date
        StringBuffer sbLast = new StringBuffer(8);
        sbLast.append(sp.getLastOrderDateYy());
        sbLast.append('-');
        sbLast.append(sp.getLastOrderDateMm());
        sbLast.append('-');
        sbLast.append(sp.getLastOrderDateDd());
        this.lastOrderDate = sbLast.toString();
        
        // Next order date
        StringBuffer sbNext = new StringBuffer(8);
        sbNext.append(sp.getNextOrderDateYy());
        sbNext.append('-');
        sbNext.append(sp.getNextOrderDateMm());
        sbNext.append('-');
        sbNext.append(sp.getNextOrderDateDd());
        this.nextOrderDate = sbNext.toString();
    }
}
