export default function Home() {
  return (
    <div className="text-white flex flex-col items-center justify-center">
      <main className="flex flex-1 flex-col justify-center items-center text-center px-6">
        <h2 className="text-4xl font-bold mb-4">Seamless Payments, Scalable Systems</h2>
        <p className="text-gray-400 mb-8 max-w-lg">
          Experience fast, secure, and reliable payments infrastructure designed for modern fintech applications.
        </p>
        <div className="space-x-4">
          <button className="bg-blue-600 hover:bg-blue-700 px-6 py-3 rounded-lg font-semibold">
            Get Started
          </button>
          <button className="border border-gray-600 px-6 py-3 rounded-lg font-semibold hover:border-gray-400">
            Learn More
          </button>
        </div>
      </main>
     
    </div>
  );
}
