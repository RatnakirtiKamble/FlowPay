import React from "react";
import { useNavigate } from "react-router-dom";
import { useAuth } from "../Context/AuthContext";

interface HeaderProps {
  onLoginClick: () => void;
  onSignupClick: () => void;
}

/**
 * Header component for navigation and authentication actions.
 *
 * Displays:
 *  - Brand logo (FlowPay) linking to Home.
 *  - Docs link (always visible).
 *  - Authenticated state: Dashboard + Logout buttons.
 *  - Unauthenticated state: Login + Signup buttons.
 */
export const Header: React.FC<HeaderProps> = ({ onLoginClick, onSignupClick }) => {
  const { user, loading, logout } = useAuth();
  const navigate = useNavigate();

  /** Navigate to dashboard if not loading */
  const handleDashboardClick = () => {
    if (!loading) navigate("/dashboard");
  };

  /** Navigate to home if not loading */
  const handleHomeClick = () => {
    if (!loading) navigate("/");
  };

  /** Call logout from auth context */
  const handleLogoutClick = () => {
    logout();
  };

  return (
    <header className="flex justify-between items-center p-6 border border-gray-800 w-3/4 rounded-full">
      {/* Logo */}
      <button className="text-3xl font-bold" onClick={handleHomeClick}>
        <span className="text-blue-500">Flow</span>
        <span className="text-white">Pay</span>
      </button>

      {/* Navigation */}
      <nav className="space-x-6 text-xl">
        <button
          className="hover:text-blue-500"
          onClick={() => navigate("/docs")}
        >
          Docs
        </button>

        {user ? (
          <>
            <button
              className="hover:text-blue-500"
              onClick={handleDashboardClick}
            >
              Dashboard
            </button>
            <button
              className="hover:text-blue-500"
              onClick={handleLogoutClick}
            >
              Logout
            </button>
          </>
        ) : (
          <>
            <button
              className="hover:text-blue-500"
              onClick={onLoginClick}
            >
              Log in
            </button>
            <button
              className="hover:text-blue-500"
              onClick={onSignupClick}
            >
              Sign up
            </button>
          </>
        )}
      </nav>
    </header>
  );
};
