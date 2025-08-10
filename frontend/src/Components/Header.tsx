import React from "react";
import { useNavigate } from "react-router-dom";
import { useAuth } from "../Context/AuthContext";

interface HeaderProps {
  onLoginClick: () => void;
  onSignupClick: () => void;
}

export const Header: React.FC<HeaderProps> = ({ onLoginClick, onSignupClick }) => {

  // 1. Destructure the user, loading, and logout function from the context
  const { user, loading, logout } = useAuth();

  const navigate = useNavigate();

  const handleDashboardClick = () =>{
    if (loading) return; // Prevent navigation if still loading
    navigate("/dashboard");
  }

  const handleLogout = () => {
    // 2. Call the logout function from the context
    logout();
  };

  const handleHome = () => {
    if (loading) return; // Prevent navigation if still loading
    navigate("/");
  };

  return (
    <header className="flex justify-between items-center p-6 border-gray-800 border w-3/4 rounded-full">
      <button 
        className="text-3xl font-bold"
        onClick={handleHome}
      >
        <span className="text-blue-500">Flow</span>
        <span className="text-white">Pay</span>
      </button>
      <nav className="space-x-6 text-xl">
      <button
          className="hover:text-blue-500"
          onClick={() => navigate("/docs")}>
          Docs
        </button>
        { user ? (
          <>
            <button
              className="hover:text-blue-500"
              onClick={(e) => {
                e.preventDefault();
                handleDashboardClick();
              } }
            >
              Dashboard
            </button>
            <button
              className="hover:text-blue-500"
              onClick={(e) => {
                e.preventDefault();
                // 3. Wire up the handler to the button's onClick event
                handleLogout();
              } }
            >
              Logout
            </button>
          </>
        ) : (
        <>
          <button
            className="hover:text-blue-500"
            onClick={(e) => {
              e.preventDefault();
              onLoginClick();
            } }
          >
             Log in
          </button><button
            className="hover:text-blue-500"
            onClick={(e) => {
              e.preventDefault();
              onSignupClick();
            } }
          >
              Sign up
            </button>
        </>
        )}
      </nav>
    </header>
  );
};
