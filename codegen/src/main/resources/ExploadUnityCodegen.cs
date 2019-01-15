﻿using System;
using System.Collections;
using UnityEngine;
using UnityEngine.Networking;
using Newtonsoft.Json;

namespace Expload.Unity.Codegen
{
    public static class ExploadTypeConverters
    {
        // signed integers

        public static sbyte ParseInt8(string elem)
        {
            if (elem.StartsWith("int8.")) {
                return sbyte.Parse(elem.Substring("int8.".Length));
            } else {
                throw new ArgumentException("Wrong format for int8 type: " + elem);
            }
        }

        public static string PrintInt8(sbyte elem)
        {
            return "int8." + elem.ToString();
        }

        public static short ParseInt16(string elem)
        {
            if (elem.StartsWith("int16.")) {
                return short.Parse(elem.Substring("int16.".Length));
            } else {
                throw new ArgumentException("Wrong format for int16 type: " + elem);
            }
        }

        public static string PrintInt16(short elem)
        {
            return "int16." + elem.ToString();
        }

        public static int ParseInt32(string elem)
        {
            if (elem.StartsWith("int32.")) {
                return int.Parse(elem.Substring("int32.".Length));
            } else {
                throw new ArgumentException("Wrong format for int32 type: " + elem);
            }
        }

        public static string PrintInt32(int elem)
        {
            return "int32." + elem.ToString();
        }

        public static long ParseInt64(string elem)
        {
            if (elem.StartsWith("int64.")) {
                return long.Parse(elem.Substring("int64.".Length));
            } else {
                throw new ArgumentException("Wrong format for int64 type: " + elem);
            }
        }

        public static string PrintInt64(long elem)
        {
            return "int64." + elem.ToString();
        }

        // bool

        public static bool ParseBool(string elem)
        {
            if (elem == "bool.true") {
                return true;
            } else if (elem == "bool.false") {
                return false;
            } else {
                throw new ArgumentException("Wrong format for bool type: " + elem);
            }
        }

        public static string PrintBool(bool elem)
        {
            return "bool." + (elem ? "true" : "false");
        }

        // utf8

        public static string ParseUtf8(string elem)
        {
            if (elem.StartsWith("utf8.")) {
                return elem.Substring("utf8.".Length);
            } else {
                throw new ArgumentException("Wrong format for utf8 type: " + elem);
            }
        }

        public static string PrintUtf8(string elem)
        {
            return "utf8." + elem;
        }

        // bytes

        public static byte[] ParseBytes(string elem)
        {
            if (elem.StartsWith("bytes.")) {
                string hex = elem.Substring("bytes.".Length);
                byte[] bytes = new byte[hex.Length / 2];
                for (int i = 0; i < hex.Length; i += 2) {
                    bytes[i / 2] = Convert.ToByte(hex.Substring(i, 2), 16);
                }
                return bytes;
            } else {
                throw new ArgumentException("Wrong format for int32 type: " + elem);
            }
        }

        public static string PrintBytes(byte[] elem)
        {
            return "bytes." + BitConverter.ToString(elem).Replace("-", "");
        }

        // null

        public static object ParseNull(string elem)
        {
            if (elem == "null") {
                return null;
            } else {
                throw new ArgumentException("Wrong format for null type: " + elem);
            }
        }

        public static string PrintNull(object elem)
        {
            return "null";
        }
    }

    public class ExploadMethodRequest
    {
        public string address { get; set; }
        public string method { get; set; }
        public string[] args { get; set; }

        public ExploadMethodRequest(string address, string method, string[] args)
        {
            this.address = address;
            this.method = method;
            this.args = args;
        }
    }

    public class ExploadResponseFinalState
    {
        public long spentWatts { get; set; }
        public long refundWatts { get; set; }
        public long totalWatts { get; set; }
        public string[] stack { get; set; }
    }

    public class ExploadResponseData
    {
        public ExploadResponseFinalState finalState { get; set; }
    }

    public class ExploadResponse
    {
	    public string transactionId { get; set; }
        public string error { get; set; }
        public string errorCode { get; set; }
        public ExploadResponseData data { get; set; }
    }

    public abstract class ProgramRequest<T>
    {
        public byte[] ProgramAddress { get; protected set; }

        public T Result { get; protected set; }
	    public string TransactionId { get; protected set; }
        public string Error { get; protected set; }
        public bool IsError { get; protected set; }

        protected ProgramRequest(byte[] programAddress)
        {
            ProgramAddress = programAddress;
            IsError = false;
            Error = "";
        }

        protected abstract T ParseResult(string elem);

        protected IEnumerator SendRequest(string method, string[] args, bool test)
        {
            var request = new ExploadMethodRequest(BitConverter.ToString(ProgramAddress).Replace("-", ""), method, args);
            string json = JsonConvert.SerializeObject(request);

            string uri = "";
            if (test) {
                uri = "localhost:8087/api/program/method-test";
            } else {
                uri = "localhost:8087/api/program/method";
            }

            UnityWebRequest www = UnityWebRequest.Put(uri, json);
            www.method = "POST";
            www.SetRequestHeader("Content-Type", "application/json");

            yield return www.SendWebRequest();

            if (www.isNetworkError || www.isHttpError)
            {
                IsError = true;
                Error = www.error + "\nHttp code: " + www.responseCode + "\nText: " + www.downloadHandler.text;
            }
            else
            {
                try
                {
                    var response = JsonConvert.DeserializeObject<ExploadResponse>(www.downloadHandler.text);
		            TransactionId = response.transactionId;
                    if (response.error.Length != 0) {
                        IsError = true;
                        Error = "Error from response: " + response.error;
                    } else if (response.data.finalState.stack.Length > 1) {
                        IsError = true;
                        Error = "Invalid method result:\n[" + String.Join(", ", response.data.finalState.stack) + "]";
                    } else if (response.data.finalState.stack.Length == 1) {
                        Result = ParseResult(response.data.finalState.stack[0]);
                    }
                }
                catch (JsonSerializationException e)
                {
                    IsError = true;
                    Error = "Invalid JSON:\n" + www.downloadHandler.text + "\nError: " + e.Message;
                }
                catch (ArgumentException e)
                {
                    IsError = true;
                    Error = "Invalid JSON:\n" + www.downloadHandler.text + "\nError: " + e.Message;
                }
            }
        }
    }
}