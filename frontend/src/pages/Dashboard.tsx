import React, { useState, useEffect } from "react";
import { useAuth } from "../Context/AuthContext";
import { ApiKeyOverlay } from "../Modals/ApiKey";

// 1. Update the Transaction interface to match the backend's Payment model
interface Transaction {
  paymentId: number;
  merchantId: number;
  amount: number;
  status: string;
  createdAt: string; // This will be an ISO date string from the server
}

function getCookie(name: string): string | null {
  const value = `; ${document.cookie}`;
  const parts = value.split(`; ${name}=`);
  if (parts.length === 2) {
    return parts.pop()?.split(";").shift() || null;
  }
  return null;
}

export default function Dashboard() {
  const { user, refreshUser } = useAuth();
  
  // --- Component State ---
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [transactionsLoading, setTransactionsLoading] = useState(true);
  const [isGenerating, setIsGenerating] = useState(false);
  const [newlyGeneratedKey, setNewlyGeneratedKey] = useState<string | null>(null);
  const [activeTab, setActiveTab] = useState<"python" | "node">("python");
  const [showText, setShowText] = useState(false);
  const [showCode, setShowCode] = useState(false);
  const [isRevoking, setIsRevoking] = useState(false); 

  // --- SDK Code Snippets ---
  const pythonCode = `# Python sample SDK code
from flowpay_sdk import FlowPayClient

client = FlowPayClient(api_key="YOUR API KEY HERE")
try:
    response = client.make_payment(amount=100.50)
    print("Payment successful:", response)
except Exception as e:
    print(f"Payment failed: {e}")`;
    const nodeCode = `// Node.js sample SDK code
    const { FlowPayClient } = require('./flowpay_sdk');
    const main = async () => {
        try {
            const client = new FlowPayClient({ apiKey: "sk_your_api_key_here" });
            const response = await client.makePayment(100.50);
            console.log("Payment successful:", response);
        } 
        catch (error) {
            console.error(\`Payment failed: \${error.message}\`);
        }
    };
    main();`;
      
  // --- API Handlers ---
  const handleRevoke = async () => {
    if (!window.confirm("Are you sure you want to revoke your API key? This action cannot be undone.")) {
      return;
    }
    setIsRevoking(true);
    try {
      const csrfToken = getCookie("XSRF-TOKEN");
      const response = await fetch("http://localhost:8080/merchant/apikey/revoke", {
        method: "POST",
        credentials: "include",
        headers: { "X-XSRF-TOKEN": csrfToken || "" },
      });
      if (!response.ok) throw new Error("Failed to revoke API key.");
      await refreshUser();
    } catch (error) {
      console.error("API Key revocation error:", error);
      alert("Could not revoke API key. Please try again.");
    } finally {
      setIsRevoking(false);
    }
  };

  const generateApiKey = async () => {
    setIsGenerating(true);
    try {
      const csrfToken = getCookie("XSRF-TOKEN");
      const response = await fetch("http://localhost:8080/merchant/apikey/generate", {
        method: "POST",
        credentials: "include",
        headers: { "X-XSRF-TOKEN": csrfToken || "" },
      });
      if (!response.ok) throw new Error("Failed to generate API key");
      const data = await response.json();
      setNewlyGeneratedKey(data.apiKey);
    } catch (error) {
      console.error("API Key generation error:", error);
      alert("Could not generate API key. Please try again.");
    } finally {
      setIsGenerating(false);
    }
  };

  const handleOverlayClose = () => {
    refreshUser();
    setNewlyGeneratedKey(null);
  };

  // --- Effects ---
  useEffect(() => {
    // Effect for fade-in animations
    setTimeout(() => setShowText(true), 500);
    setTimeout(() => setShowCode(true), 1000);
  }, []);

  // 2. Add a new useEffect to fetch transactions when the user is available
  useEffect(() => {
    const fetchTransactions = async () => {
      if (!user) return; // Don't fetch if the user isn't loaded yet

      setTransactionsLoading(true);
      try {
        const csrfToken = getCookie("XSRF-TOKEN");
        const response = await fetch("http://localhost:8080/merchant/payments", {
          credentials: "include",
          headers: {
            "X-XSRF-TOKEN": csrfToken || "",
          },
        });
        if (!response.ok) {
          throw new Error("Failed to fetch transactions.");
        }
        const data: Transaction[] = await response.json();
        setTransactions(data);
      } catch (error) {
        console.error("Error fetching transactions:", error);
        // Optionally set an error state here to show in the UI
      } finally {
        setTransactionsLoading(false);
      }
    };

    fetchTransactions();
  }, [user]); // Re-run this effect if the user object changes

  if (!user) {
    return null; 
  }

  return (
    <div className="flex flex-col bg-transparent text-white min-h-screen">
      {/* Banner Section */}
      <section className="h-[calc(100vh-220px)] flex items-center px-16 text-white select-none">
        <div
          className={`w-1/2 pr-12 transition-opacity duration-1000 ease-out ${
            showText ? "opacity-100" : "opacity-0"
          }`}
        >
          <h1 className="text-5xl font-extrabold tracking-widest">
            All your needs, in one place.
          </h1>
        </div>
        <div
          className={`w-1/2 bg-black border-blue-500 border rounded-lg shadow-lg p-6 font-mono text-sm max-h-[60vh] overflow-auto transition-opacity duration-1000 ease-out delay-500 ${
            showCode ? "opacity-100" : "opacity-0"
          }`}
        >
          <div className="flex space-x-4 mb-4">
            <button
              className={`px-4 py-2 rounded-t-lg border-b-2 ${
                activeTab === "python"
                  ? "border-blue-500 text-blue-400 font-semibold"
                  : "border-transparent text-gray-400 hover:text-white"
              }`}
              onClick={() => setActiveTab("python")}
            >
              Python
            </button>
            <button
              className={`px-4 py-2 rounded-t-lg border-b-2 ${
                activeTab === "node"
                  ? "border-blue-500 text-blue-400 font-semibold"
                  : "border-transparent text-gray-400 hover:text-white"
              }`}
              onClick={() => setActiveTab("node")}
            >
              Node.js
            </button>
          </div>
          <pre className="whitespace-pre-wrap">
            {activeTab === "python" ? pythonCode : nodeCode}
          </pre>
        </div>
      </section>

      {/* Dashboard Heading */}
      <h1 className="text-5xl font-bold text-white text-center mt-8 mb-6">
        Dashboard
      </h1>

      {/* Profile Section */}
      <section className="max-w-4xl mx-auto w-full p-6 bg-white/10 backdrop-blur-md rounded-lg mb-10">
        <h2 className="text-2xl font-semibold border-b border-blue-500 pb-2 mb-4">
          Profile Details
        </h2>
        <p className="mb-2">
          <span className="font-semibold text-blue-400">Name:</span> {user.name}
        </p>
        <p>
          <span className="font-semibold text-blue-400">Email:</span> {user.email}
        </p>
      </section>

      {/* API Key Section */}
      <section className="max-w-4xl mx-auto w-full p-6 bg-white/10 backdrop-blur-md rounded-lg mb-10">
        <h2 className="text-2xl font-semibold border-b border-blue-500 pb-2 mb-4">
          API Key
        </h2>
        <div className="flex items-center space-x-3">
          {user.apiKeyExists ? (
            <>
              <code className="bg-white/20 text-blue-300 px-4 py-2 rounded-lg break-words flex-grow">
                sk_********************
              </code>
              <button
                onClick={handleRevoke}
                disabled={isRevoking}
                className="bg-blue-500 hover:bg-blue-700 disabled:bg-gray-500 transition-colors px-4 py-2 rounded-lg font-semibold"
              >
                {isRevoking ? "Revoking..." : "Revoke"}
              </button>
            </>
          ) : (
            <button
              onClick={generateApiKey}
              disabled={isGenerating}
              className="bg-blue-500 hover:bg-blue-600 disabled:bg-gray-500 transition-colors px-6 py-3 rounded-lg font-bold"
            >
              {isGenerating ? "Generating..." : "Generate API Key"}
            </button>
          )}

          {/* Demo Button */}
          {user.apiKeyExists ?
          <a
            
            href="/demo"
            target="_blank"
            rel="noopener noreferrer"
            className="ml-auto bg-yellow-500 hover:bg-yellow-600 text-black font-semibold px-6 py-3 rounded-lg transition"
          >
            Demo
          </a> : null
          }
        </div>
      </section>


      {/* Transactions & Balance Section */}
      <section className="max-w-4xl mx-auto w-full p-6 bg-white/10 backdrop-blur-md rounded-lg mb-16">
        <h2 className="text-2xl font-semibold border-b border-blue-500 pb-2 mb-4">
          Transactions & Balance
        </h2>
        <p className="text-3xl font-extrabold text-white mb-6">
          Current Balance: ${user.balance.toFixed(2)}
        </p>
        
        {/* 3. Update the table to be fully dynamic */}
        {transactionsLoading ? (
          <p className="text-gray-300">Loading transactions...</p>
        ) : transactions.length === 0 ? (
          <p className="text-gray-300">No payments yet.</p>
        ) : (
          <table className="w-full text-sm text-left border-collapse table-auto">
            <thead>
              <tr className="border-b border-white/30">
                <th className="py-2 px-3">Transaction ID</th>
                <th className="py-2 px-3">Date</th>
                <th className="py-2 px-3">Amount</th>
                <th className="py-2 px-3">Status</th>
              </tr>
            </thead>
            <tbody>
              {transactions.map((txn) => (
                <tr
                  key={txn.paymentId}
                  className="even:bg-white/10 hover:bg-white/20 transition-colors cursor-default"
                >
                  <td className="py-2 px-3">{txn.paymentId}</td>
                  <td className="py-2 px-3">{new Date(txn.createdAt).toLocaleDateString()}</td>
                  <td className="py-2 px-3">${txn.amount.toFixed(2)}</td>
                  <td
                    className={`py-2 px-3 font-semibold ${
                      txn.status === "Success"
                        ? "text-green-400"
                        : "text-yellow-400" // Default for other statuses
                    }`}
                  >
                    {txn.status}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </section>

      {newlyGeneratedKey && (
        <ApiKeyOverlay apiKey={newlyGeneratedKey} onClose={handleOverlayClose} />
      )}
    </div>
  );
}
