import React from "react";

interface HeaderProps {
  onLoginClick: () => void;
  onSignupClick: () => void;
}

export const Header: React.FC<HeaderProps> = ({ onLoginClick, onSignupClick }) => {
  return (
    <header className="flex justify-between items-center p-6 border-gray-800 border w-3/4 rounded-full">
      <h1 className="text-3xl font-bold">
        <span className="text-blue-500">Flow</span>
        <span className="text-white">Pay</span>
      </h1>
      <nav className="space-x-6 text-xl">
      <button
          className="hover:text-blue-500">
          About Us
        </button>
        <button
          className="hover:text-blue-500"
          onClick={(e) => {
            e.preventDefault();
            onLoginClick();
          }}
        >
          Log in
        </button>
        <button
          className="hover:text-blue-500"
          onClick={(e) => {
            e.preventDefault();
            onSignupClick();
          }}
        >
          Sign up
        </button>
      </nav>
    </header>
  );
};
