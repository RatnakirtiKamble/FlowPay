import React, { useEffect, useState } from "react";

type Product = {
  id: number;
  title: string;
  price: number;
  image: string;
  description: string;
};

export default function DemoMerchantSite() {
    const [apiKey, setApiKey] = useState<string | null>(null);
    const [hasPrompted, setHasPrompted] = useState(false);
    const [products, setProducts] = useState<Product[]>([]);
    const [loadingProducts, setLoadingProducts] = useState(true);
    const [paymentStatus, setPaymentStatus] = useState<string | null>(null);
    const [payingProductId, setPayingProductId] = useState<number | null>(null);

    useEffect(() => {
        if (!apiKey && !hasPrompted) {
        const enteredKey = prompt("Enter your API Key:");
        setHasPrompted(true);
        if (enteredKey && enteredKey.trim() !== "") {
            setApiKey(enteredKey.trim());
        }
        }
    }, [apiKey, hasPrompted]);

    // Fetch products
    useEffect(() => {
        if (!apiKey) return;

        setLoadingProducts(true);
        fetch("https://fakestoreapi.com/products")
        .then((res) => res.json())
        .then((data) => {
            setProducts(data);
            setLoadingProducts(false);
        })
        .catch(() => {
            alert("Failed to fetch products.");
            setLoadingProducts(false);
        });
    }, [apiKey]);

    const handlePayNow = async (product: Product) => {
        if (!apiKey) {
        alert("Missing API key.");
        return;
        }

        setPaymentStatus(null);
        setPayingProductId(product.id);

        try {
        const res = await fetch(`${import.meta.env.VITE_BACKEND_URL}/payments`, {
            method: "POST",
            headers: {
            "Content-Type": "application/json",
            "X-API-Key": apiKey,
            },
            body: JSON.stringify({
            paymentAmount: product.price,
            }),
        });

        if (res.ok) {
            setPaymentStatus(`Payment for $${product.price.toFixed(2)} successful!`);
        } else {
            setPaymentStatus("Payment failed. Please try again.");
        }
        } catch (err) {
        setPaymentStatus("Error contacting payment server.");
        } finally {
        setPayingProductId(null);
        }
    };

    if (!apiKey && hasPrompted) {
        return (
          <div className="flex items-center justify-center min-h-screen bg-gray-900 text-white p-4">
            <h2 className="text-center">
              API Key is required. Please reload the page and enter a valid API key.
            </h2>
          </div>
        );
      }

  return (
    <div className="min-h-screen bg-gray-50 text-gray-900 rounded-4xl">
      {/* Header */}
      <header className="bg-blue-600 text-white p-4 flex justify-between items-center shadow-lg rounded-t-4xl">
        <h1 className="text-3xl font-bold cursor-pointer select-none">Demo Store</h1>
        <div>
          <span className="text-sm font-mono bg-blue-800 px-2 py-1 rounded">{`API Key: ${apiKey}`}</span>
        </div>
      </header>

      {/* Payment status */}
      {paymentStatus && (
        <div className="bg-green-100 text-green-800 max-w-4xl mx-auto p-4 my-4 rounded shadow text-center font-semibold">
          {paymentStatus}
        </div>
      )}

      {/* Products grid */}
      <main className="max-w-6xl mx-auto p-6">
        {loadingProducts ? (
          <p className="text-center text-gray-600">Loading products...</p>
        ) : (
          <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-8">
            {products.map((product) => (
              <div
                key={product.id}
                className="bg-white rounded-lg shadow-md p-4 flex flex-col justify-between"
              >
                <img
                  src={product.image}
                  alt={product.title}
                  className="h-48 w-full object-contain mb-4"
                />
                <h3 className="text-lg font-semibold mb-2">{product.title}</h3>
                <p className="text-gray-700 mb-4 line-clamp-3">{product.description}</p>
                <div className="flex items-center justify-between mt-auto">
                  <span className="font-bold text-xl">${product.price.toFixed(2)}</span>
                  <button
                    disabled={payingProductId === product.id}
                    onClick={() => handlePayNow(product)}
                    className={`bg-yellow-500 hover:bg-yellow-600 text-black font-semibold px-4 py-2 rounded transition disabled:opacity-50 disabled:cursor-not-allowed`}
                  >
                    {payingProductId === product.id ? "Processing..." : "Pay Now"}
                  </button>
                </div>
              </div>
            ))}
          </div>
        )}
      </main>

      {/* Footer */}
      <footer className="bg-gray-800 text-gray-400 p-4 text-center text-sm mt-12">
        Demo merchant site powered by Fake Store API & Flowpay
      </footer>
    </div>
  );
}
