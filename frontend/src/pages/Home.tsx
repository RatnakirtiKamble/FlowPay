import { useNavigate } from "react-router-dom";
import { useAuth } from "../Context/AuthContext";

export default function Home() {
  const { user, openLoginModal } = useAuth();
  const navigate = useNavigate();

  const handleGetStartedClick = () => {
    if (user) {
      navigate("/dashboard");
    } else {
      openLoginModal();
    }
  };

  const handleLearnMore = () => {
    navigate("/docs");
  }

  return (
    <div className="flex flex-col items-center justify-center h-full min-h-[calc(100vh-210px)] text-white px-6 bg-gradient-to-r from-black via-gray-900 to-black">
      <main className="flex flex-col justify-center items-center text-center max-w-lg mx-auto">
        <h2 className="text-4xl font-bold mb-4">Seamless Payments, Scalable Systems</h2>
        <p className="text-gray-400 mb-8">
          Experience fast, secure, and reliable payments infrastructure designed for modern fintech applications.
        </p>
        <div className="space-x-4">
          <button 
            onClick={handleGetStartedClick}
            className="bg-blue-600 hover:bg-blue-700 px-6 py-3 rounded-lg font-semibold"
          >
            Get Started
          </button>
          <button 
            className="border border-gray-600 px-6 py-3 rounded-lg font-semibold hover:border-gray-400"
            onClick = {handleLearnMore}  
          >
            
            Learn More
          </button>
        </div>
      </main>
    </div>
  );
}
