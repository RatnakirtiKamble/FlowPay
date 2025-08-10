import React, { useState, useEffect } from 'react';

interface ApiKeyOverlayProps {
  apiKey: string;
  onClose: () => void;
}

export const ApiKeyOverlay: React.FC<ApiKeyOverlayProps> = ({ apiKey, onClose }) => {
  const [copyButtonText, setCopyButtonText] = useState('Copy');

  const handleCopy = () => {
    navigator.clipboard.writeText(apiKey);
    setCopyButtonText('Copied!');
    setTimeout(() => setCopyButtonText('Copy'), 2000); 
  };

  useEffect(() => {
    const handleEsc = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        onClose();
      }
    };
    window.addEventListener('keydown', handleEsc);
    return () => {
      window.removeEventListener('keydown', handleEsc);
    };
  }, [onClose]);

  return (
    <div
      className="fixed inset-0 bg-black/70 flex justify-center items-center z-50 backdrop-blur-sm"
      onClick={onClose}
    >
      <div
        className="bg-gray-900 border border-blue-500 rounded-lg p-8 w-full max-w-md relative shadow-2xl text-white"
        onClick={(e) => e.stopPropagation()} // Prevent closing when clicking inside
      >
        <h2 className="text-2xl mb-4 font-bold text-blue-400">Your New API Key</h2>
        <p className="text-yellow-400 bg-yellow-900/30 p-3 rounded-md border border-yellow-600 mb-6">
          Please store this API key safely. You will not be able to see it again.
        </p>

        <div className="flex items-center space-x-3 mb-6">
          <code className="bg-black text-green-400 px-4 py-2 rounded-lg break-all flex-grow select-all">
            {apiKey}
          </code>
          <button
            onClick={handleCopy}
            className="bg-blue-600 hover:bg-blue-700 transition-colors px-4 py-2 rounded-lg font-semibold w-24"
          >
            {copyButtonText}
          </button>
        </div>
        
        <button
          onClick={onClose}
          className="w-full bg-gray-600 hover:bg-gray-700 transition-colors py-2 rounded-lg font-bold"
        >
          Done
        </button>
      </div>
    </div>
  );
};