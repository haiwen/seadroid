package com.seafile.seadroid2.framework.model

import android.util.Log
import java.security.MessageDigest
import java.security.cert.CertificateParsingException
import java.security.cert.X509Certificate
import java.util.Date
import javax.security.auth.x500.X500Principal

/**
 * PKIX X.509 v3 certificate wrapper.
 *
 * Reference cadroid: https://github.com/bitfireAT/cadroid
 */
class CertificateInfo(private val certificate: X509Certificate) {

    fun getSubjectName(): String =
        certificate.subjectX500Principal.getName(X500Principal.RFC1779)

    fun getSubjectAltNames(): Array<String>? {
        return try {
            val altNames = mutableListOf<String>()
            val subjectAltNames = certificate.subjectAlternativeNames
            if (subjectAltNames != null) {
                for (asn1Name in subjectAltNames) {
                    val type = (asn1Name[0] as? Int) ?: continue
                    val value = try {
                        asn1Name[1] as? String ?: "?"
                    } catch (e: Exception) {
                        Log.w(DEBUG_TAG, "Couldn't cast alternative subject name to String", e)
                        "?"
                    }
                    altNames.add("$value [$type]")
                }
            }
            altNames.toTypedArray()
        } catch (e: CertificateParsingException) {
            Log.w(DEBUG_TAG, "Couldn't parse Subject Alternative Names from certificate", e)
            null
        }
    }

    fun getSerialNumber(): String = certificate.serialNumber.toString(16)

    fun getSignature(algorithm: String): String {
        return try {
            val digest = MessageDigest.getInstance(algorithm)
            digest.update(certificate.encoded)
            val bytes = digest.digest()
            buildString(bytes.size * 2) {
                for (byte in bytes) {
                    append(Integer.toHexString(byte.toInt() and 0xFF))
                }
            }
        } catch (e: Exception) {
            Log.e(DEBUG_TAG, "Couldn't calculate certificate digest", e)
            e.message ?: ""
        }
    }

    fun getNotBefore(): Date = certificate.notBefore

    fun getNotAfter(): Date = certificate.notAfter

    fun isCurrentlyValid(): Boolean = runCatching { certificate.checkValidity() }.isSuccess

    private companion object {
        private const val DEBUG_TAG = "CertificateInfo"
    }
}
