package com.seafile.seadroid2.framework.motion_photo;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;
import com.adobe.internal.xmp.options.ParseOptions;
import com.adobe.internal.xmp.options.SerializeOptions;

import org.w3c.dom.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import java.io.*;
import java.nio.charset.StandardCharsets;

/**
 * Utilities to merge original XMP XML with Motion Photo fields and Container:Directory
 */
public final class MotionXmpMerger {

    private static final String NS_X = "adobe:ns:meta/";
    private static final String NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    private static final String NS_GCAM = "http://ns.google.com/photos/1.0/camera/";
    private static final String NS_CONTAINER = "http://ns.google.com/photos/1.0/container/";
    private static final String NS_ITEM = "http://ns.google.com/photos/1.0/container/item/";
    private static final String NS_HDRGM = "http://ns.adobe.com/hdr-gain-map/1.0/";

    private MotionXmpMerger() {
    }

    /**
     * Merge original XMP (as XML string) with Motion Photo Container and GCamera fields.
     *
     * @param originalXmpString original XMP RDF/XML string, or null if none
     * @param primaryMime       e.g. "image/jpeg" or "image/heic"
     * @param mp4Length         Length of mp4 bytes (for Container:Item Item:Length of MotionPhoto)
     * @param microVideoOffset  MicroVideoOffset value to write (e.g. 8 + mp4Length)
     * @param paddingPrimary    Padding value for primary item (e.g. 8)
     * @return final RDF/XML XMP string (with xpacket wrapper) ready to set into XmlBox
     * @throws Exception on parse/serialize errors
     */
    public static String mergeXmpXml(String originalXmpString,
                                     String primaryMime,
                                     int mp4Length,
                                     long microVideoOffset,
                                     int paddingPrimary) throws Exception {

        Document doc;
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder db = dbf.newDocumentBuilder();

        if (originalXmpString != null && !originalXmpString.trim().isEmpty()) {
            // try parse original; if parse fails, fall back to new doc
            try (InputStream is = new ByteArrayInputStream(originalXmpString.getBytes(StandardCharsets.UTF_8))) {
                doc = db.parse(is);
            } catch (Exception e) {
                // fallback to fresh
                doc = createEmptyXmpDoc(db);
            }
        } else {
            doc = createEmptyXmpDoc(db);
        }

        // find or create x:xmpmeta > rdf:RDF > rdf:Description
        Element xmpMeta = getOrCreateChildNS(doc, doc, "x", "xmpmeta", NS_X);
        Element rdf = getOrCreateChildNS(doc, xmpMeta, "rdf", "RDF", NS_RDF);
        Element desc = null;

        // find rdf:Description (may be multiple). We choose the first.
        NodeList descList = rdf.getElementsByTagNameNS(NS_RDF, "Description");
        if (descList != null && descList.getLength() > 0) {
            desc = (Element) descList.item(0);
        } else {
            // create rdf:Description with empty rdf:about=""
            desc = doc.createElementNS(NS_RDF, "rdf:Description");
            desc.setAttributeNS(NS_RDF, "rdf:about", "");
            rdf.appendChild(desc);
        }

        // ensure namespace declarations exist on rdf:Description (add if missing)
        ensureNamespaceAttr(desc, "hdrgm", NS_HDRGM);
        ensureNamespaceAttr(desc, "GCamera", NS_GCAM);
        ensureNamespaceAttr(desc, "Container", NS_CONTAINER);
        ensureNamespaceAttr(desc, "Item", NS_ITEM);

        // ---- Merge/Update GCamera attrs ----
        // Set or overwrite GCamera:MotionPhoto, MotionPhotoVersion, MicroVideo, MicroVideoOffset
        desc.setAttributeNS(NS_GCAM, "GCamera:MotionPhoto", "1");
        desc.setAttributeNS(NS_GCAM, "GCamera:MotionPhotoVersion", "1");
        desc.setAttributeNS(NS_GCAM, "GCamera:MicroVideo", "1");
        desc.setAttributeNS(NS_GCAM, "GCamera:MicroVideoOffset", Long.toString(microVideoOffset));
        // Note: you can add presentation timestamp if you have it, e.g. GCamera:MotionPhotoPresentationTimestampUs

        // ---- Handle Container:Directory merging ----
        Element containerDir = null;
        NodeList containerNodes = desc.getElementsByTagNameNS(NS_CONTAINER, "Directory");
        if (containerNodes != null && containerNodes.getLength() > 0) {
            containerDir = (Element) containerNodes.item(0);
            // we'll modify it in place: update MotionPhoto entry and Primary padding
            mergeContainerDirectory(doc, containerDir, primaryMime, mp4Length, paddingPrimary);
        } else {
            // create new Container:Directory rdf:Seq with primary + motion entries
            Element containerEl = doc.createElementNS(NS_CONTAINER, "Container:Directory");
            Element seq = doc.createElementNS(NS_RDF, "rdf:Seq");

            // primary item
            Element liPrimary = doc.createElementNS(NS_RDF, "rdf:li");
            liPrimary.setAttributeNS(NS_RDF, "rdf:parseType", "Resource");
            Element itemPrimary = doc.createElementNS(NS_CONTAINER, "Container:Item");
            // use Item namespace attributes
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Mime", primaryMime);
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Semantic", "Primary");
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Length", "0");
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Padding", Integer.toString(paddingPrimary));
            liPrimary.appendChild(itemPrimary);

            // video item
            Element liVideo = doc.createElementNS(NS_RDF, "rdf:li");
            liVideo.setAttributeNS(NS_RDF, "rdf:parseType", "Resource");
            Element itemVideo = doc.createElementNS(NS_CONTAINER, "Container:Item");
            itemVideo.setAttributeNS(NS_ITEM, "Item:Mime", "video/mp4");
            itemVideo.setAttributeNS(NS_ITEM, "Item:Semantic", "MotionPhoto");
            itemVideo.setAttributeNS(NS_ITEM, "Item:Length", Integer.toString(mp4Length));
            liVideo.appendChild(itemVideo);

            seq.appendChild(liPrimary);
            seq.appendChild(liVideo);
            containerEl.appendChild(seq);
            desc.appendChild(containerEl);
        }

        // Serialize DOM to string but wrap with xpacket wrapper as in your example
        String rdfXml = domToString(xmpMeta); // but we want entire xmpmeta element as string

        // Build full xpacket wrapper: <?xpacket begin...?> + x:xmpmeta ... + <?xpacket end='w'?>
        StringBuilder sb = new StringBuilder();
        sb.append("<?xpacket begin=\"\uFEFF\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n");
        sb.append(rdfXml).append("\n");
        sb.append("<?xpacket end='w'?>");

        return sb.toString();
    }

    // --- helpers ---

    private static Document createEmptyXmpDoc(DocumentBuilder db) {
        Document d = db.newDocument();
        // create x:xmpmeta root
        Element xmpMeta = d.createElementNS(NS_X, "x:xmpmeta");
        // set x:xmptk default
        xmpMeta.setAttribute("x:xmptk", "Adobe XMP Core 5.1.0-jc003");
        d.appendChild(xmpMeta);

        Element rdf = d.createElementNS(NS_RDF, "rdf:RDF");
        xmpMeta.appendChild(rdf);

        Element desc = d.createElementNS(NS_RDF, "rdf:Description");
        desc.setAttributeNS(NS_RDF, "rdf:about", "");
        rdf.appendChild(desc);

        return d;
    }

    private static Element getOrCreateChildNS(Document doc, Node parent, String prefix, String localName, String ns) {
        NodeList list = ((Element) parent).getElementsByTagNameNS(ns, localName);
        if (list != null && list.getLength() > 0) {
            return (Element) list.item(0);
        } else {
            Element el = doc.createElementNS(ns, prefix + ":" + localName);
            parent.appendChild(el);
            return el;
        }
    }

    private static void ensureNamespaceAttr(Element desc, String prefix, String nsUri) {
        // If namespace prefix not declared on this element, declare it
        String attrName = "xmlns:" + prefix;
        if (!desc.hasAttribute(attrName)) {
            desc.setAttribute(attrName, nsUri);
        }
    }

    private static void mergeContainerDirectory(Document doc, Element containerDir, String primaryMime, int mp4Length, int paddingPrimary) {
        // Expect containerDir contains rdf:Seq -> rdf:li* each containing Container:Item with Item:* attributes
        NodeList seqList = containerDir.getElementsByTagNameNS(NS_RDF, "Seq");
        Element seq = null;
        if (seqList != null && seqList.getLength() > 0) seq = (Element) seqList.item(0);
        if (seq == null) {
            // create seq and add default primary + motion
            seq = doc.createElementNS(NS_RDF, "rdf:Seq");
            containerDir.appendChild(seq);
        }

        // iterate li children, find MotionPhoto entry and Primary; collect others to keep
        NodeList liList = seq.getElementsByTagNameNS(NS_RDF, "li");
        boolean foundMotion = false;
        boolean foundPrimary = false;

        for (int i = 0; i < liList.getLength(); i++) {
            Element li = (Element) liList.item(i);
            // find Container:Item inside this li
            NodeList items = li.getElementsByTagNameNS(NS_CONTAINER, "Item");
            if (items == null || items.getLength() == 0) continue;
            Element item = (Element) items.item(0);

            // semantic may be in Item:Semantic attribute (Item namespace)
            String semantic = item.getAttributeNS(NS_ITEM, "Semantic");
            if (semantic == null || semantic.isEmpty()) {
                // try attribute without namespace
                semantic = item.getAttribute("Item:Semantic");
            }
            if ("MotionPhoto".equals(semantic)) {
                // update Length to mp4Length
                item.setAttributeNS(NS_ITEM, "Item:Length", Integer.toString(mp4Length));
                foundMotion = true;
            } else if ("Primary".equals(semantic)) {
                // ensure Padding set to paddingPrimary
                item.setAttributeNS(NS_ITEM, "Item:Padding", Integer.toString(paddingPrimary));
                // ensure mime updated if provided
                if (primaryMime != null && !primaryMime.isEmpty()) {
                    item.setAttributeNS(NS_ITEM, "Item:Mime", primaryMime);
                }
                foundPrimary = true;
            } else {
                // keep as is
            }
        }

        if (!foundMotion) {
            // append motion item at end
            Element liVideo = doc.createElementNS(NS_RDF, "rdf:li");
            liVideo.setAttributeNS(NS_RDF, "rdf:parseType", "Resource");
            Element itemVideo = doc.createElementNS(NS_CONTAINER, "Container:Item");
            itemVideo.setAttributeNS(NS_ITEM, "Item:Mime", "video/mp4");
            itemVideo.setAttributeNS(NS_ITEM, "Item:Semantic", "MotionPhoto");
            itemVideo.setAttributeNS(NS_ITEM, "Item:Length", Integer.toString(mp4Length));
            liVideo.appendChild(itemVideo);
            seq.appendChild(liVideo);
        }
        if (!foundPrimary) {
            // insert a primary as first li
            Element liPrimary = doc.createElementNS(NS_RDF, "rdf:li");
            liPrimary.setAttributeNS(NS_RDF, "rdf:parseType", "Resource");
            Element itemPrimary = doc.createElementNS(NS_CONTAINER, "Container:Item");
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Mime", primaryMime != null ? primaryMime : "image/jpeg");
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Semantic", "Primary");
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Length", "0");
            itemPrimary.setAttributeNS(NS_ITEM, "Item:Padding", Integer.toString(paddingPrimary));
            liPrimary.appendChild(itemPrimary);
            // insert at beginning of seq
            Node first = seq.getFirstChild();
            if (first != null) seq.insertBefore(liPrimary, first);
            else seq.appendChild(liPrimary);
        }
    }

    private static String domToString(Element xmpMetaElement) throws TransformerException {
        TransformerFactory tf = TransformerFactory.newInstance();
        Transformer transformer = tf.newTransformer();

        // pretty printing: optional
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        // Keep xmlns declarations and prefixes
        DOMSource source = new DOMSource(xmpMetaElement);
        StringWriter sw = new StringWriter();
        StreamResult result = new StreamResult(sw);
        transformer.transform(source, result);
        return sw.toString();
    }

    // Utility: optional: convert XMPMeta -> original xml string (if you have XMPMeta)
    public static String xmpMetaToString(XMPMeta xmpMeta) throws XMPException {
        SerializeOptions opts = new SerializeOptions();
        opts.setOmitPacketWrapper(false);
        opts.setUseCompactFormat(true);
        return XMPMetaFactory.serializeToString(xmpMeta, opts);
    }
}
