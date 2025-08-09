export default function Header() {
  return (
    <header className="flex justify-between items-center p-6 border-gray-800 border w-3/4 rounded-full">
      <h1 className="text-2xl font-bold text-blue-500">FlowPay</h1>
      <nav className="space-x-6">
        <a href="#" className="hover:text-gray-400">Home</a>
        <a href="#" className="hover:text-gray-400">Transactions</a>
        <a href="#" className="hover:text-gray-400">Settings</a>
      </nav>
    </header>
  );
}
