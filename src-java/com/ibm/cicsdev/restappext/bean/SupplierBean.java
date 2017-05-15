package com.ibm.cicsdev.restappext.bean;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.ibm.cicsdev.restappext.generated.Supplier;

/**
 * Simple data access bean, used to serialise the required data to JSON using
 * the JAX-RS interfaces.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "supplier")
public class SupplierBean
{
    @XmlElement(name = "supplierId")
    private int iSupplierId;
    
    @XmlElement(name = "name")
    private String name;
    
    /**
     * Constructor to initialise this instance using values provided by
     * the supplied {@link Supplier} instance.
     * 
     * @param s The object used to initialise this instance.
     */
    public SupplierBean(Supplier s) {
    
        // Pull all of the values into the internal representation
        this.iSupplierId = s.getSupplierId();
        this.name = s.getSupplierName();
    }

}
