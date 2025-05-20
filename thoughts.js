import React, { useState, useEffect, useRef } from 'react';
import { CheckCircle, AlertCircle, RefreshCw, ChevronDown, ChevronRight, Sparkles, X, Copy } from 'lucide-react';

const SimplifiedThoughtTree = () => {
  const [thoughts, setThoughts] = useState([]);
  const [isGenerating, setIsGenerating] = useState(false);
  const [expandedNodes, setExpandedNodes] = useState({});
  const [selectedThought, setSelectedThought] = useState(null);
  const [jsonViewerState, setJsonViewerState] = useState({});
  
  // Status types
  const STATUS = {
    RUNNING: 'running',
    DONE: 'done',
    FAILED: 'failed'
  };
  
  // Generate a random JSON object for demo purposes
  const generateRandomData = (id, content) => {
    return {
      id,
      content,
      timestamp: new Date().toISOString(),
      status: Math.random() > 0.7 ? "error" : Math.random() > 0.4 ? "success" : "pending",
      metrics: {
        duration: Math.floor(Math.random() * 1000),
        tokens: Math.floor(Math.random() * 500) + 100,
        confidence: Number((Math.random() * 0.5 + 0.5).toFixed(2))
      },
      parameters: {
        temperature: Number((Math.random() * 0.7 + 0.3).toFixed(1)),
        topP: 0.9,
        model: "strwbry-v1"
      },
      ...(Math.random() > 0.5 ? {
        results: Array.from({ length: Math.floor(Math.random() * 5) + 1 }, (_, i) => ({
          index: i,
          value: Math.random().toFixed(3),
          label: `Result ${i+1}`
        }))
      } : {})
    };
  };
  
  // Add a new root thought
  const addThought = (content, parentId = null, status = STATUS.RUNNING) => {
    const id = `thought-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
    
    const newThought = {
      id,
      content,
      children: [],
      parentId,
      status,
      details: generateRandomData(id, content),
      isNew: true
    };
    
    if (parentId) {
      // Add as a child to existing parent
      setThoughts(prev => {
        const updateWithChild = thoughts => thoughts.map(thought => {
          if (thought.id === parentId) {
            return { ...thought, children: [...thought.children, newThought] };
          } else if (thought.children.length > 0) {
            return { ...thought, children: updateWithChild(thought.children) };
          }
          return thought;
        });
        
        return updateWithChild(prev);
      });
      
      // Auto-expand parent
      setExpandedNodes(prev => ({ ...prev, [parentId]: true }));
    } else {
      // Add as a new root thought
      setThoughts(prev => [newThought, ...prev]);
    }
    
    // Auto-remove the "new" flag after animation
    setTimeout(() => {
      setThoughts(prev => {
        const removeNewFlag = thoughts => thoughts.map(thought => {
          if (thought.id === id) {
            return { ...thought, isNew: false };
          } else if (thought.children.length > 0) {
            return { ...thought, children: removeNewFlag(thought.children) };
          }
          return thought;
        });
        
        return removeNewFlag(prev);
      });
    }, 600);
    
    return id;
  };
  
  // Update a thought's status
  const updateStatus = (id, status) => {
    setThoughts(prev => {
      const updateStatusInTree = thoughts => thoughts.map(thought => {
        if (thought.id === id) {
          return { ...thought, status, statusChanged: true };
        } else if (thought.children.length > 0) {
          return { ...thought, children: updateStatusInTree(thought.children) };
        }
        return thought;
      });
      
      return updateStatusInTree(prev);
    });
    
    // Clear the status change animation flag after delay
    setTimeout(() => {
      setThoughts(prev => {
        const clearFlag = thoughts => thoughts.map(thought => {
          if (thought.id === id) {
            return { ...thought, statusChanged: false };
          } else if (thought.children.length > 0) {
            return { ...thought, children: clearFlag(thought.children) };
          }
          return thought;
        });
        
        return clearFlag(prev);
      });
    }, 600);
    
    // Auto-collapse completed thoughts
    if (status === STATUS.DONE || status === STATUS.FAILED) {
      setTimeout(() => {
        setExpandedNodes(prev => ({ ...prev, [id]: false }));
      }, 1000);
    }
  };
  
  // Toggle expansion state of a node
  const toggleNode = (id, event) => {
    if (event) event.stopPropagation();
    setExpandedNodes(prev => ({ ...prev, [id]: !prev[id] }));
  };
  
  // Toggle JSON viewer node expansion
  const toggleJsonNode = (path) => {
    setJsonViewerState(prev => ({ ...prev, [path]: !prev[path] }));
  };
  
  // Find a thought by ID
  const findThought = (id, thoughts = []) => {
    for (const thought of thoughts) {
      if (thought.id === id) return thought;
      if (thought.children.length > 0) {
        const found = findThought(id, thought.children);
        if (found) return found;
      }
    }
    return null;
  };
  
  // Handle click on a thought to view details
  const handleViewDetails = (id, event) => {
    // Ignore if clicking on a button
    if (event.target.closest('button')) return;
    
    const thought = findThought(id, thoughts);
    if (thought) {
      setSelectedThought(thought);
      // Reset JSON viewer state
      setJsonViewerState({});
    }
  };
  
  // Generate a demo sequence of thoughts
  const generateDemo = () => {
    if (isGenerating) return;
    
    setIsGenerating(true);
    setThoughts([]);
    setExpandedNodes({});
    setSelectedThought(null);
    
    // First root thought
    setTimeout(() => {
      const root1 = addThought("Analyzing visualization request", null, STATUS.RUNNING);
      
      setTimeout(() => {
        // Child of first root
        const child1 = addThought("Processing input parameters", root1, STATUS.RUNNING);
        
        // Second root thought (parallel)
        setTimeout(() => {
          const root2 = addThought("Preparing thought tree structure", null, STATUS.RUNNING);
          
          // Complete first child
          setTimeout(() => {
            updateStatus(child1, STATUS.DONE);
            
            // Add another child to first root
            setTimeout(() => {
              const child2 = addThought("Validating user requirements", root1, STATUS.RUNNING);
              
              // Add child to second root
              setTimeout(() => {
                const child3 = addThought("Implementing view transitions", root2, STATUS.RUNNING);
                
                // Add third root
                setTimeout(() => {
                  const root3 = addThought("Creating JSON detail viewer", null, STATUS.RUNNING);
                  
                  setTimeout(() => {
                    // Update statuses
                    updateStatus(child2, STATUS.DONE);
                    updateStatus(child3, STATUS.DONE);
                    
                    setTimeout(() => {
                      // Add child to third root
                      const child4 = addThought("Supporting arbitrary JSON objects", root3, STATUS.RUNNING);
                      
                      setTimeout(() => {
                        // Complete all
                        updateStatus(root2, STATUS.DONE);
                        updateStatus(child4, STATUS.DONE);
                        updateStatus(root3, STATUS.DONE);
                        updateStatus(root1, STATUS.DONE);
                        
                        setTimeout(() => {
                          addThought("Completed implementation", null, STATUS.DONE);
                          setIsGenerating(false);
                        }, 1000);
                      }, 1000);
                    }, 800);
                  }, 1000);
                }, 800);
              }, 800);
            }, 800);
          }, 1000);
        }, 1000);
      }, 800);
    }, 500);
  };
  
  // Auto-start on first load
  useEffect(() => {
    generateDemo();
  }, []);
  
  // Render a thought with its children
  const renderThought = (thought, depth = 0) => {
    const isExpanded = expandedNodes[thought.id] === true;
    const hasChildren = thought.children && thought.children.length > 0;
    
    // Get status icon based on thought status
    const getStatusIcon = () => {
      const animClass = thought.statusChanged ? "status-change-animation" : "";
      
      switch (thought.status) {
        case STATUS.RUNNING:
          return <RefreshCw size={16} className={`text-blue-400 animate-spin ${animClass}`} />;
        case STATUS.DONE:
          return <CheckCircle size={16} className={`text-green-400 ${animClass}`} />;
        case STATUS.FAILED:
          return <AlertCircle size={16} className={`text-red-400 ${animClass}`} />;
        default:
          return null;
      }
    };
    
    return (
      <div 
        key={thought.id} 
        className={`thought-node ${thought.isNew ? 'thought-appear' : ''} ${thought.statusChanged ? 'status-change' : ''}`}
        style={{ paddingLeft: `${depth * 24}px`, marginBottom: '6px' }}
      >
        <div 
          className="flex items-start cursor-pointer"
          onClick={(e) => handleViewDetails(thought.id, e)}
        >
          {/* Expand/collapse button */}
          <div className={`w-6 h-6 flex items-center justify-center mr-1 mt-1 transition-transform duration-300 ${isExpanded ? 'rotate-0' : '-rotate-90'}`}>
            {hasChildren ? (
              <button 
                onClick={(e) => toggleNode(thought.id, e)}
                className="text-gray-400 hover:text-white transition-colors"
              >
                {isExpanded ? <ChevronDown size={18} /> : <ChevronRight size={18} />}
              </button>
            ) : (
              <div className="w-4" />
            )}
          </div>
          
          {/* Thought content */}
          <div 
            className={`py-1.5 px-3 rounded-md flex items-center transition-all duration-300 border ${
              depth === 0 
                ? 'bg-gradient-to-r from-pink-900/40 to-indigo-900/40 text-pink-100 border-pink-900/50 hover:border-pink-500/50' 
                : 'bg-gradient-to-r from-indigo-900/30 to-blue-900/30 text-blue-100 border-indigo-900/30 hover:border-indigo-500/50'
            }`}
          >
            <div className="mr-2">{getStatusIcon()}</div>
            <div className="flex items-center">
              {thought.status === STATUS.RUNNING && depth === 0 && (
                <Sparkles size={14} className="mr-2 text-pink-400 opacity-70" />
              )}
              <span>{thought.content}</span>
            </div>
          </div>
        </div>
        
        {/* Render children if expanded */}
        {isExpanded && hasChildren && (
          <div className="children-container animate-fadeIn">
            {thought.children.map(child => renderThought(child, depth + 1))}
          </div>
        )}
      </div>
    );
  };
  
  // Simplified JSON viewer for any JSON structure
  const JsonViewer = ({ data, path = "root", depth = 0 }) => {
    const isExpanded = jsonViewerState[path] !== false; // Default to expanded for first level
    
    // Handle different value types
    if (data === null) return <span className="text-gray-400">null</span>;
    if (data === undefined) return <span className="text-gray-400">undefined</span>;
    
    if (typeof data !== 'object') {
      // Handle primitives
      if (typeof data === 'string') return <span className="text-green-300">"{data}"</span>;
      if (typeof data === 'number') return <span className="text-blue-300">{data}</span>;
      if (typeof data === 'boolean') return <span className="text-yellow-300">{data.toString()}</span>;
      return <span>{String(data)}</span>;
    }
    
    // Handle arrays and objects
    const isArray = Array.isArray(data);
    const isEmpty = isArray ? data.length === 0 : Object.keys(data).length === 0;
    
    if (isEmpty) {
      return <span className="text-gray-400">{isArray ? "[]" : "{}"}</span>;
    }
    
    return (
      <div className="json-node">
        <div 
          className="flex items-center cursor-pointer"
          onClick={() => toggleJsonNode(path)}
        >
          <button className="mr-1 focus:outline-none">
            {isExpanded ? 
              <ChevronDown size={14} className="text-gray-400" /> : 
              <ChevronRight size={14} className="text-gray-400" />
            }
          </button>
          <span className="text-gray-300">
            {isArray ? `Array[${data.length}]` : `Object{${Object.keys(data).length}}`}
          </span>
        </div>
        
        {isExpanded && (
          <div className="pl-4 border-l border-gray-700 mt-1">
            {isArray ? (
              // Render array items
              data.map((item, idx) => (
                <div key={`${path}-${idx}`} className="my-1">
                  <span className="text-gray-500 mr-2">{idx}:</span>
                  <JsonViewer data={item} path={`${path}.${idx}`} depth={depth + 1} />
                </div>
              ))
            ) : (
              // Render object properties
              Object.entries(data).map(([key, value]) => (
                <div key={`${path}-${key}`} className="my-1">
                  <span className="text-purple-300 mr-2">{key}:</span>
                  <JsonViewer data={value} path={`${path}.${key}`} depth={depth + 1} />
                </div>
              ))
            )}
          </div>
        )}
      </div>
    );
  };
  
  // JSON Detail Modal
  const JsonDetailModal = ({ thought, onClose }) => {
    if (!thought) return null;
    
    // Copy JSON to clipboard
    const copyJson = () => {
      const json = JSON.stringify(thought.details, null, 2);
      navigator.clipboard.writeText(json);
    };
    
    return (
      <div className="fixed inset-0 bg-gray-900/80 backdrop-blur-sm z-50 flex items-center justify-center p-4">
        <div className="bg-gray-800 rounded-lg shadow-xl max-w-4xl w-full max-h-[90vh] flex flex-col">
          {/* Header */}
          <div className="p-4 border-b border-gray-700 flex items-center justify-between">
            <h3 className="text-xl font-bold text-white">
              {thought.content}
            </h3>
            <div className="flex items-center gap-2">
              <button
                onClick={copyJson}
                className="p-2 rounded-md hover:bg-gray-700 transition-colors text-gray-400 hover:text-white"
                aria-label="Copy JSON"
              >
                <Copy size={18} />
              </button>
              <button 
                onClick={onClose}
                className="p-2 rounded-md hover:bg-gray-700 transition-colors text-gray-400 hover:text-white"
                aria-label="Close"
              >
                <X size={18} />
              </button>
            </div>
          </div>
          
          {/* Scrollable content */}
          <div className="flex-1 overflow-auto p-4 font-mono text-sm">
            <div className="bg-gray-900 rounded-md p-4">
              <JsonViewer data={thought.details} />
            </div>
          </div>
          
          {/* Footer */}
          <div className="p-2 bg-gray-800 border-t border-gray-700 text-xs text-gray-500 text-center">
            Click on objects and arrays to expand/collapse
          </div>
        </div>
      </div>
    );
  };

  return (
    <div className="h-screen w-full bg-gradient-to-b from-gray-900 to-indigo-900 flex flex-col overflow-hidden p-4">
      {/* Header */}
      <div className="text-center mb-6 mt-4">
        <h1 className="text-2xl font-bold text-white">strwbry.ai Thought Tree</h1>
        <p className="text-gray-300">Click on any thought to view details</p>
      </div>
      
      {/* Tree container */}
      <div className="flex-1 flex justify-center">
        <div className="bg-gray-800/50 backdrop-blur-sm rounded-lg p-6 shadow-xl max-w-2xl w-full overflow-auto">
          <div className="font-mono text-sm">
            {thoughts.map(thought => renderThought(thought))}
            
            {thoughts.length === 0 && (
              <div className="text-center text-gray-400 py-12">
                No thoughts yet
              </div>
            )}
          </div>
        </div>
      </div>
      
      {/* Generate button */}
      <div className="flex justify-center mt-6 mb-4">
        <button
          onClick={generateDemo}
          disabled={isGenerating}
          className="px-6 py-3 bg-gradient-to-r from-pink-500 to-indigo-600 rounded-lg text-white font-medium text-lg shadow-lg disabled:opacity-50"
        >
          {isGenerating ? "Generating Thoughts..." : "Generate Thought Tree"}
        </button>
      </div>
      
      {/* Logo */}
      <div className="absolute bottom-6 right-6 opacity-50">
        <div className="text-4xl">🍓</div>
      </div>
      
      {/* Detail modal */}
      {selectedThought && (
        <JsonDetailModal 
          thought={selectedThought} 
          onClose={() => setSelectedThought(null)} 
        />
      )}
      
      {/* CSS for animations */}
      <style jsx>{`
        @keyframes thoughtAppear {
          0% { opacity: 0; transform: translateY(-15px) scale(0.9); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        
        .thought-appear {
          animation: thoughtAppear 0.5s cubic-bezier(0.2, 0.8, 0.2, 1) forwards;
        }
        
        @keyframes statusPulse {
          0% { transform: scale(1); }
          50% { transform: scale(1.05); box-shadow: 0 0 15px rgba(255, 255, 255, 0.2); }
          100% { transform: scale(1); }
        }
        
        .status-change {
          animation: statusPulse 0.6s ease-in-out;
        }
        
        @keyframes statusIconPop {
          0% { transform: scale(1); }
          50% { transform: scale(1.5); }
          100% { transform: scale(1); }
        }
        
        .status-change-animation {
          animation: statusIconPop 0.5s cubic-bezier(0.34, 1.56, 0.64, 1);
        }
        
        @keyframes fadeIn {
          from { opacity: 0; }
          to { opacity: 1; }
        }
        
        .animate-fadeIn {
          animation: fadeIn 0.3s ease-out;
        }
        
        .children-container {
          position: relative;
          transition: all 0.3s ease-out;
        }
      `}</style>
    </div>
  );
};

export default SimplifiedThoughtTree;