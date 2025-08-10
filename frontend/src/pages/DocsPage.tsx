import { useState } from "react";
import { motion } from "framer-motion";

const codeSnippets = {
  pythonSdk: `# Python sample SDK code
from flowpay_sdk import FlowPayClient

def main():
    try:
        client = FlowPayClient(api_key="sk_your_api_key_here")
        response = client.make_payment(100.50)
        print("Payment successful:", response)
    except Exception as e:
        print(f"Payment failed: {e}")

if __name__ == "__main__":
    main()
`,

  nodeJsSdk: `// Node.js sample SDK code
const { FlowPayClient } = require('./flowpay_sdk');

const main = async () => {
    try {
        const client = new FlowPayClient({ apiKey: "sk_your_api_key_here" });
        const response = await client.makePayment(100.50);
        console.log("Payment successful:", response);
    } catch (error) {
        console.error(\`Payment failed: \${error.message}\`);
    }
};

main();
`,
};

export default function DocsPage() {
  const [selectedSnippet, setSelectedSnippet] = useState<"pythonSdk" | "nodeJsSdk">("pythonSdk");

  return (
    <div className="min-h-screen bg-gradient-to-r from-black via-gray-900 to-black text-white px-6 py-12 max-w-5xl mx-auto font-sans">
      <motion.h1
        className="text-5xl font-extrabold mb-8 text-center"
        initial={{ y: -50, opacity: 0 }}
        animate={{ y: 0, opacity: 1 }}
        transition={{ duration: 0.7 }}
      >
        Developer Documentation & SDK Usage
      </motion.h1>

      <motion.section
        className="mb-12"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.8, duration: 0.7 }}
      >
        <h2 className="text-3xl font-bold mb-4">About the Payment SDKs</h2>
        <p className="text-gray-400 leading-relaxed max-w-prose mx-auto">
          Our SDKs simplify integration with the payment gateway. Use the Python or Node.js SDKs to securely process payments with minimal setup.
          The SDK handles authentication, request formatting, and error handling to ensure seamless transactions.
        </p>
      </motion.section>

      <motion.section
        className="mb-12"
        initial={{ opacity: 0, x: -50 }}
        animate={{ opacity: 1, x: 0 }}
        transition={{ delay: 1.4, duration: 0.7 }}
      >
        <h2 className="text-3xl font-bold mb-4">Quick Start Instructions</h2>
        <ol className="list-decimal list-inside text-gray-300 max-w-prose mx-auto space-y-2">
          <li>Install the SDK for your platform: <code>pip install flowpay-sdk</code> or <code>npm install flowpay-sdk</code>.</li>
          <li>Import the SDK in your project and instantiate the client with your secret API key.</li>
          <li>Call <code>make_payment(amount)</code> or <code>makePayment(amount)</code> to process payments.</li>
          <li>Handle success and error responses to update your UI accordingly.</li>
        </ol>
      </motion.section>

      <motion.section
        className="mb-12"
        initial={{ opacity: 0, x: 50 }}
        animate={{ opacity: 1, x: 0 }}
        transition={{ delay: 2, duration: 0.7 }}
      >
        <h2 className="text-3xl font-bold mb-6">Code Examples</h2>

        <div className="flex justify-center mb-6 space-x-6">
          <button
            onClick={() => setSelectedSnippet("pythonSdk")}
            className={`px-4 py-2 rounded-lg font-semibold ${
              selectedSnippet === "pythonSdk" ? "bg-blue-600" : "bg-gray-800 hover:bg-gray-700"
            }`}
          >
            Python SDK
          </button>
          <button
            onClick={() => setSelectedSnippet("nodeJsSdk")}
            className={`px-4 py-2 rounded-lg font-semibold ${
              selectedSnippet === "nodeJsSdk" ? "bg-blue-600" : "bg-gray-800 hover:bg-gray-700"
            }`}
          >
            Node.js SDK
          </button>
        </div>

        <pre className="bg-gray-900 rounded-lg p-6 overflow-x-auto max-h-[400px] text-sm leading-relaxed whitespace-pre-wrap font-mono">
          {codeSnippets[selectedSnippet]}
        </pre>
      </motion.section>

    </div>
  );
}
